use axum::{
    Json, Router,
    extract::{MatchedPath, Path, Query, State},
    http::{self, Request},
    routing::{get, post},
    serve,
};
use axum_macros::debug_handler;
use csv::ReaderBuilder;
// use regex::Regex;
use b64_ct::{STANDARD, ToBase64};
use little_exif::{
    exif_tag::ExifTag, exif_tag_format::RATIONAL64U, filetype::FileExtension, metadata::Metadata,
};
use pcre2::bytes::{Match, Regex};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tracing::info_span;
use std::{
    cmp::{Ordering, PartialOrd},
    collections::{BTreeMap, BTreeSet, HashSet},
    error::Error,
    fmt,
    fmt::Display,
    io::Read,
    str::Chars,
    sync::Arc,
};
use tokio::net::TcpListener;
use tower_http::{trace::TraceLayer, cors::{Any, CorsLayer}};

#[derive(Debug, Deserialize)]
struct Place {
    city: String,
    city_ascii: String,
    lat: f32,
    lng: f32,
    country: String,
    iso2: String,
    iso3: String,
    admin_name: String,
    capital: String,
    population: u64,
    id: u64,
}

#[derive(Debug, Serialize, Clone)]
struct City {
    id: u64,
    country: String,
    capital: String,
    population: u64,
    lat: f32,
    lng: f32,
}

#[derive(Serialize, Clone)]
struct Coord {
    lat: f32,
    lng: f32,
    label: String,
}

impl Eq for Coord {}

impl PartialEq for Coord {
    fn eq(&self, other: &Self) -> bool {
        self.lat == other.lat
            && self.lng == other.lng
            && self.label.to_uppercase() == other.label.to_uppercase()
    }
}

impl Ord for Coord {
    fn cmp(&self, other: &Self) -> Ordering {
        if let Some(order) = self.partial_cmp(other) {
            order
        } else {
            panic!("invalid coordinates");
        }
    }
}

impl PartialOrd for Coord {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.lat.partial_cmp(&other.lat) {
            Some(Ordering::Less) => Some(Ordering::Less),
            Some(Ordering::Greater) => Some(Ordering::Greater),
            Some(Ordering::Equal) => match self.lng.partial_cmp(&other.lng) {
                Some(Ordering::Less) => Some(Ordering::Less),
                Some(Ordering::Greater) => Some(Ordering::Greater),
                Some(Ordering::Equal) => match self
                    .label
                    .to_uppercase()
                    .partial_cmp(&other.label.to_uppercase())
                {
                    Some(Ordering::Less) => Some(Ordering::Less),
                    Some(Ordering::Greater) => Some(Ordering::Greater),
                    Some(Ordering::Equal) => Some(Ordering::Equal),
                    None => None,
                },
                None => None,
            },
            None => None,
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct Record<K, V: fmt::Debug>
where
    K: Ord + PartialOrd + Eq + PartialEq + fmt::Debug,
{
    key: K,
    value: V,
}

impl<K, V: fmt::Debug> Record<K, V>
where
    K: Ord + PartialOrd + Eq + PartialEq + fmt::Debug,
{
    pub fn new(key: K, value: V) -> Self {
        Self { key, value }
    }
    pub fn key(&self) -> &K {
        &self.key
    }

    pub fn value(&self) -> &V {
        &self.value
    }
}

impl<K: Ord + PartialOrd + Eq + PartialEq + fmt::Debug, V: fmt::Debug> Eq for Record<K, V> {}

impl<K, V: fmt::Debug> PartialEq for Record<K, V>
where
    K: Ord + PartialOrd + Eq + PartialEq + fmt::Debug,
{
    fn eq(&self, other: &Self) -> bool {
        self.key.eq(&other.key)
    }
}

impl<K, V: fmt::Debug> PartialOrd for Record<K, V>
where
    K: Ord + PartialOrd + Eq + PartialEq + fmt::Debug,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.key.partial_cmp(&other.key)
    }
}

impl<K, V: fmt::Debug> Ord for Record<K, V>
where
    K: Ord + PartialOrd + Eq + PartialEq + fmt::Debug,
{
    fn cmp(&self, other: &Self) -> Ordering {
        self.key.cmp(&other.key)
    }
}

impl<K, V: fmt::Debug> Display for Record<K, V>
where
    K: Ord + PartialOrd + Eq + PartialEq + fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}     {:?}", self.key, self.value)
    }
}

fn parse_csv() -> Result<(BTreeMap<String, City>, Vec<Coord>), Box<dyn Error>> {
    let mut cities = BTreeMap::<String, City>::new();
    let mut coords = Vec::<Coord>::new();
    let reader = ReaderBuilder::new()
        .has_headers(true)
        .from_path("data/worldcities.csv")?;

    for record in reader.into_deserialize() {
        match record {
            Ok(place) => {
                let Place {
                    id,
                    city: name,
                    capital,
                    lat,
                    lng,
                    country,
                    population,
                    ..
                } = place;

                cities.insert(
                    name.clone(),
                    City {
                        id,
                        country,
                        capital,
                        population,
                        lat,
                        lng,
                    },
                );

                coords.push(Coord {
                    lat,
                    lng,
                    label: name,
                });
            }
            Err(e) => {
                // println!("{e:?}");
                // return Err(Box::new(e));
                continue;
            }
        }
    }

    coords.sort();

    Ok((cities, coords))
}

#[derive(Debug, Deserialize, Serialize, Clone)]
struct Title {
    rendered: String,
}

#[derive(Debug, Deserialize, Serialize, Clone, Default)]
struct Html {
    rendered: String,
    protected: bool,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
struct Post {
    id: u64,
    slug: String,
    link: String,
    author: u64,
    title: Title,
    content: Html,
    featured_media: u64,
    excerpt: Html,
    categories: Vec<u64>,
    tags: Vec<u64>,
}

#[derive(Debug, Deserialize, Serialize)]
struct Media {
    alt_text: String,
    mime_type: String,
    source_url: String
}

#[derive(Debug, Deserialize, Serialize, PartialEq, PartialOrd, Eq, Ord, Clone)]
struct Label {
    id: u64,
    name: String,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
struct Image {
    b64: String,
    width: u64,
    height: u64,
    alt: String,
    coords: Option<Record<String, (f32, f32)>>
}

#[derive(Debug, Serialize, Clone)]
struct PostData {
    id: u64,
    title: String,
    slug: String,
    excerpt: String,
    image: Image,
    link: String,
    categories: BTreeMap<String, u64>,
    tags: BTreeMap<String, u64>,
    coords: BTreeSet<Record<String, (f32, f32)>>,
}

#[derive(Clone)]
struct AppState {
    ids: Vec<u64>,
    cities: BTreeMap<String, City>,
    coords: Vec<Coord>,
    post_coords: Vec<Record<String, (f32, f32)>>,
    posts: Vec<Post>,
    post_data: Vec<PostData>,
    categories: Vec<Label>,
    tags: Vec<Label>,
    slug_map: Arc<BTreeMap<String, Html>>
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    print!("Parsing cities...");
    let (cities, coords) = parse_csv()?;
    println!("done");
    let mut post_coords = Vec::<Record<String, (f32, f32)>>::new();
    print!("Fetching post data...");
    let posts =
    reqwest::get("https://ozimage.com.au/wp-json/wp/v2/posts?_fields=author,id,title,slug,excerpt,content,featured_media,link,categories,tags").await?.json::<Vec<Post>>().await?;
    println!("done");
    // println!("{posts:?}");
    print!("Fetching categories...");
    let categories =
        reqwest::get("https://ozimage.com.au/wp-json/wp/v2/categories?_fields=id,name")
            .await?
            .json::<Vec<Label>>()
            .await?;
    println!("done");
    print!("Fetching tags...");
    let tags = reqwest::get("https://ozimage.com.au/wp-json/wp/v2/tags?_fields=id,name")
        .await?
        .json::<Vec<Label>>()
        .await?;
    println!("done");

    println!("Caching post data...");
    let mut post_data = Vec::<PostData>::new();
    let re = Regex::new(r"(\p{Lu}\p{Ll}+)(\s\p{Lu}\p{Ll}*)*")?;

    // <img decoding="async" class="rsImg rsMainSlideImage lazyloaded" data-src="https://ozimage.com.au/wp-content/uploads/Gallery-and-wine-tasting-room-2.jpg" alt="" src="https://ozimage.com.au/wp-content/uploads/Gallery-and-wine-tasting-room-2.jpg" style="--smush-placeholder-width: 1095px; --smush-placeholder-aspect-ratio: 1095/602; width: 805px; height: 443px;">

    let img_re = Regex::new(
        r#"<img.*?(width="(?<width>\d+?)".*?height="(?<height>\d+?)".*?)?(?:data-)?src="(?<url>http.+/(?<filename>.+\.(?<ext>png|jpg|jpeg)))".*?alt="(?<alt>.*?)?".*?/?>"#,
    )?;
    // println!("{:?}", posts[0].content.rendered.as_bytes());
    for post in &posts {
        let Post {
            id,
            title,
            slug,
            excerpt,
            content,
            featured_media,
            link,
            categories: cat_list,
            tags: tag_list,
            ..
        } = post.clone();
        let title = title.rendered;

        print!("\t ...extracting main image...");
        let mut b64 = String::new();
        let mut width: u64 = 500;
        let mut height: u64 = 500;
        let mut alt = "".to_string();
        let mut place_name = "".to_string();
        let mut img_coords: Option<Record<String, (f32, f32)>> = None;
        let mut lon: Option<f32> = None;
        let mut lat: Option<f32> = None;
        let mut mime = "image/jpeg";

        let excerpt = excerpt.rendered;
        let cat_data = BTreeMap::<String, u64>::from_iter(
            categories
                .iter()
                .map(|cat| (cat.name.clone(), cat.id.clone()))
                .filter(|(_, v)| cat_list.contains(v)),
        );
        let tag_data = BTreeMap::<String, u64>::from_iter(
            tags.iter()
                .map(|tag| (tag.name.clone(), tag.id.clone()))
                .filter(|(_, v)| tag_list.contains(v)),
        );
        
        let mut coords = BTreeSet::<Record<String, (f32, f32)>>::new();
        
        for result in re.captures_iter(&post.excerpt.rendered.as_bytes()) {
            let caps = result?;
            for i in 0..(caps.len() - 1) {
                let match_ = str::from_utf8(&caps[i]).unwrap_or_default();
                
                if let Some((_, city)) = cities.iter().find(|(n, c)| {
                        *match_ == *c.capital.as_str()
                        || *match_ == *c.country.as_str()
                        // || match tags.iter().find(|label| *label.name.as_str() == *c.country.as_str() || *label.name.as_str() == *c.capital.as_str()) {
                        //     Some(_) => true,
                        //     None => false
                        // }
                        // || match categories.iter().find(|label| *label.name.as_str() == *c.country.as_str() || *label.name.as_str() == *c.capital.as_str()) {
                        //     Some(_) => true,
                        //     None => false
                        // }
                        || *match_ == *n.as_str()
                        
                }) {
                    let coord = Record::new(match_.to_string(), (city.lat, city.lng));
                    coords.insert(coord.clone());
                    post_coords.push(coord);
                    place_name = match_.to_string();
                }
            }
        }

        let match_ = img_re.captures(content.rendered.as_bytes())?;

        if let Some(captures) = match_ {
            // println!("\ncaptures: {:?}", &captures);
            let url = str::from_utf8(&captures["url"])?;
            // println!("url: {url}");
            if let Some(a) = captures.name("alt") {
                alt = str::from_utf8(&a.as_bytes())?.to_string();
            }
            if let Some(w) = captures.name("width") {
                if let Ok(n) = u64::from_str_radix(str::from_utf8(&w.as_bytes())?, 10) {
                    width = n;
                }
            }
            if let Some(h) = captures.name("height") {
                if let Ok(n) = u64::from_str_radix(str::from_utf8(&h.as_bytes())?, 10) {
                    height = n;
                }
            }

            let media = reqwest::get(format!("https://ozimage.com.au/wp-json/wp/v2/media/{featured_media}?_fields=alt_text,mime_type,source_url")).await?.json::<Media>().await?;

            let res = reqwest::get(media.source_url).await?;

            // println!("\n{res:?}");
            let mime_header = res.headers().get("content-type").cloned();
            if let Some(hv) = &mime_header {
                let s = hv.to_str()?;
                mime = s;
            }
                let bytes = res.bytes().await?;

                let buf = &bytes.clone().into_iter().collect::<Vec<u8>>();
                let ext = match str::from_utf8(&captures["ext"]) {
                    Ok("jpg") | Ok("jpeg") => Some(FileExtension::JPEG),
                    Ok("png") => Some(FileExtension::PNG {
                        as_zTXt_chunk: false,
                    }),
                    Ok("webp") => Some(FileExtension::WEBP),
                    _ => None,
                };
                if let Some(fe) = ext {
                    let mut metadata = match Metadata::new_from_vec(buf, fe) {
                        Ok(meta) => Some(meta),
                        Err(e) => {
                            println!("{e:?}");
                            None
                        }
                    };
                    // println!("metadata: {metadata:?}");
                    if let Some(meta) = &mut metadata {
                        if let Some(ExifTag::GPSLongitude(lon_)) = &mut meta
                            .get_tag(&ExifTag::GPSLongitude(RATIONAL64U::new()))
                            .next()
                        {
                            if let Some(ExifTag::GPSLongitudeRef(lon_ref)) = meta
                                .get_tag(&ExifTag::GPSLongitudeRef("".to_string()))
                                .next()
                            {
                                // println!("lon_ref: {lon_ref}");
                                let m = if *lon_ref == "E".to_string() {1f32} else {-1f32};
                                // println!("lon: {lon_:?}");
                                lon = Some(lon_[0].nominator as f32 * m);
                            }
                        }
                    
                    if let Some(ExifTag::GPSLatitude(lat_)) = &mut meta
                            .get_tag(&ExifTag::GPSLatitude(RATIONAL64U::new()))
                            .next()
                    {
                        if let Some(ExifTag::GPSLatitudeRef(lat_ref)) = meta
                            .get_tag(&ExifTag::GPSLatitudeRef("".to_string()))
                            .next()
                        {
                            // println!("lat_ref: {lat_ref}");
                            let m = if *lat_ref == "N".to_string() {1f32} else {-1f32};
                            // println!("lat: {lat_:?}");
                            lat = Some(lat_[0].nominator as f32 * m);
                        }
                    }
                }
            }

                print!("\t...converting to base64...");
                b64.push_str(
                    format!(
                        "data:image/{};base64,",
                        mime
                    )
                    .as_str(),
                );
                
                b64.push_str(&bytes.to_base64(STANDARD));
                if b64 == "" {
                    // println!("data for image with alt {alt:?} missing");
                }
                println!("done");
            }
            if let (Some(lt), Some(ln)) = (lat, lon) {
                img_coords = Some(Record::<String, (f32, f32)>::new(place_name, (lt, ln)));
            }
            
            let image = Image {
                        b64,
                        width,
                        height,
                        alt,
                        coords: img_coords
                    };

        post_data.push(PostData {
            id,
            title,
            slug,
            excerpt,
            image,
            link,
            categories: cat_data,
            tags: tag_data,
            coords,
        });
    
    }
    let slug_map = Arc::new(BTreeMap::<String, Html>::from_iter(
        posts.iter().map(|d| (d.slug.clone(), d.content.clone())),
    ));

    let ids = post_data.iter().map(|d| d.id).collect::<Vec<u64>>();
    post_data.sort_by(|d1, d2| d1.id.cmp(&d2.id));

    let state = Arc::<AppState>::new(AppState {
        ids,
        cities,
        coords,
        post_coords,
        posts,
        post_data,
        categories,
        tags,
        slug_map,
    });

    println!("done");

    print!("Initializing server...");

    let cors = CorsLayer::new()
        .allow_methods([http::Method::GET])
        .allow_origin(Any);

    tracing_subscriber::fmt::init();
    // let trace = TraceLayer::new_for_http()
    // .make_span_with(|request: &Request<_>| {
    //                 // Log the matched route's path (with placeholders not filled in).
    //                 // Use request.uri() or OriginalUri if you want the real path.
    //                 let matched_path = request
    //                     .extensions()
    //                     .get::<MatchedPath>()
    //                     .map(MatchedPath::as_str);

    //                 info_span!(
    //                     "http_request",
    //                     method = ?request.method(),
    //                     matched_path,
    //                     some_other_field = tracing::field::Empty,
    //                 )
    //             })
    //             .on_request(|request, _| println!("{request:?}"))
    //             .on_response(|response, _, _| println!("{response:?}"));

    let app = Router::<Arc<AppState>>::new()
        .route("/posts", get(get_posts))
        .route("/posts/ids", get(get_post_ids))
        .route("/post/data/{id}", get(get_post_data))
        .route("/post/{slug}", get(get_post))
        .route("/posts/coords", get(get_coords))
        .with_state(state)
        .route_layer(cors);
        // .layer(trace);

    println!("done");
    let listener = TcpListener::bind("0.0.0.0:8080").await?;
    tracing::debug!("listening on {}", listener.local_addr().unwrap());
    serve(listener, app).await?;
    println!("finished");
    Ok(())

}

#[debug_handler]
async fn get_posts(State(state): State<Arc<AppState>>) -> Json<Vec<PostData>> {
    let data = state.post_data.clone();
    let value = Json(data);
    value
}

#[debug_handler]
async fn get_post_ids(State(state): State<Arc<AppState>>) -> Json<Vec<u64>> {
    Json(state.ids.clone())
}

#[debug_handler]
async fn get_post(State(state): State<Arc<AppState>>, Path(slug): Path<String>) -> Json<String> {
    let post = state
        .slug_map
        .get(&slug)
        .unwrap_or(&Html {
            rendered: "".to_string(),
            protected: true,
        })
        .clone();

    Json(post.rendered)
}

#[debug_handler]
async fn get_post_data(State(state): State<Arc<AppState>>, Path(id): Path<u64>) -> Json<PostData> {
    let idx = state
        .post_data
        .binary_search_by(|d| d.id.cmp(&id))
        .expect("Post not found");
    Json(state.post_data[idx].clone())
}

#[debug_handler]
async fn get_coords(State(state): State<Arc<AppState>>) -> Json<Vec<Record<String, (f32, f32)>>> {
    Json(state.post_coords.clone())
}