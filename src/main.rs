mod entity;

use axum::{
    Json, Router,
    extract::{MatchedPath, Path, Query, State},
    http::{self, Request},
    routing::{get, post},
    serve,
};
use axum_macros::debug_handler;
use bytes::Bytes;
use chrono::TimeZone;
use csv::ReaderBuilder;
// use regex::Regex;
use b64_ct::{STANDARD, ToBase64};
use little_exif::{
    exif_tag::ExifTag,
    exif_tag_format::{INT32U, RATIONAL64U},
    filetype::FileExtension,
    metadata::Metadata,
};
use pcre2::bytes::{Match, Regex};
use serde::{Deserialize, Serialize};

use sea_orm::{
    ActiveValue::Set,
    ColumnTrait, ConnectOptions, ConnectionTrait, Database, DatabaseConnection, DbBackend, DbErr,
    EntityTrait, JsonField, QueryFilter, QueryOrder, QuerySelect, QueryTrait, SqlxSqliteConnector,
    sea_query::{QueryStatement, SelectStatement},
};
use sea_orm_migration::manager::SchemaManager;

use entity::{
    city::Entity as Cities, city::Model as City, image::Image, menu_item::Entity as MenuItems,
    menu_item::Model as MenuItem, post::Entity as Posts, post::Model as Post,
};
use tracing::Subscriber;

use std::{
    cmp::{Ordering, PartialOrd},
    collections::{BTreeMap, BTreeSet, HashSet},
    error::Error,
    fmt::{self, Display},
    io::Read,
    ops::Index,
    path::absolute,
    str::{Chars, FromStr},
    sync::Arc,
};
use tokio::net::TcpListener;
use tower_http::{
    cors::{Any, CorsLayer},
    trace::TraceLayer,
};

use entity::record::Record;

use dotenv::{dotenv, from_filename};

use random;
use random::Source;

use crate::entity::{menu_item, post};

const DATABASE_URL: &str = "sqlite::memory:";
const DB_NAME: &str = "ozimage";

async fn open_db() -> Result<DatabaseConnection, DbErr> {
    let db = SqlxSqliteConnector::connect(ConnectOptions::new(DATABASE_URL)).await?;
    Ok(db)
}

#[derive(Debug, Deserialize, Clone)]
struct Place {
    pub id: u32,
    pub city: String,
    pub city_ascii: String,
    pub lat: f32,
    pub lng: f32,
    pub country: String,
    pub iso2: String,
    pub iso3: String,
    pub admin_name: String,
    pub capital: String,
    pub population: u32,
}

// #[derive(Debug, Serialize, Clone)]
// struct City {
// id: u32,
// name: String,
// country: String,
// capital: String,
// admin_name: String,
// population: u32,
// lat: f32,
// lng: f32,
// }

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

// #[derive(Debug, Serialize, Deserialize, Hash, Clone)]
// struct Record<K, V: fmt::Debug>
// where
//     K: Ord + PartialOrd + Eq + PartialEq + fmt::Debug,
// {
//     key: K,
//     value: V,
// }

// impl<K, V: fmt::Debug> Record<K, V>
// where
//     K: Ord + PartialOrd + Eq + PartialEq + fmt::Debug,
// {
//     pub fn new(key: K, value: V) -> Self {
//         Self { key, value }
//     }
//     pub fn key(&self) -> &K {
//         &self.key
//     }

//     pub fn value(&self) -> &V {
//         &self.value
//     }
// }

// impl<K: Ord + PartialOrd + Eq + PartialEq + fmt::Debug, V: fmt::Debug> Eq for Record<K, V> {}

// impl<K, V: fmt::Debug> PartialEq for Record<K, V>
// where
//     K: Ord + PartialOrd + Eq + PartialEq + fmt::Debug,
// {
//     fn eq(&self, other: &Self) -> bool {
//         self.key.eq(&other.key)
//     }
// }

// impl<K, V: fmt::Debug> PartialOrd for Record<K, V>
// where
//     K: Ord + PartialOrd + Eq + PartialEq + fmt::Debug,
// {
//     fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
//         self.key.partial_cmp(&other.key)
//     }
// }

// impl<K, V: fmt::Debug> Ord for Record<K, V>
// where
//     K: Ord + PartialOrd + Eq + PartialEq + fmt::Debug,
// {
//     fn cmp(&self, other: &Self) -> Ordering {
//         self.key.cmp(&other.key)
//     }
// }

// impl<K, V: fmt::Debug> Display for Record<K, V>
// where
//     K: Ord + PartialOrd + Eq + PartialEq + fmt::Debug,
// {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{:?}     {:?}", self.key, self.value)
//     }
// }

// #[derive(Debug, Serialize, Deserialize)]
// struct Link {

// }

// #[derive(Debug, Deserialize, Serialize)]
// struct MenuItem {
//     id: u32,
//     title: Record<String, String>,
//     status: String,
//     url: String,
//     attr_title: String,
//     description: String,
//     parent: u32,
//     menu_order: u32,
//     target: String,
//     classes: Vec<String>,
//     _links:

// }

async fn parse_csv(
    db: DatabaseConnection,
) -> Result<(Vec<entity::city::Model>, Vec<Coord>), Box<dyn Error>> {
    let mut cities = Vec::<entity::city::Model>::new();
    let mut coords = Vec::<Coord>::new();
    let reader = ReaderBuilder::new()
        .has_headers(true)
        .from_path("data/worldcities.csv")?;

    for record in reader.into_deserialize::<Place>() {
        // println!("record: {record:#?}");
        match record {
            Ok(place) => {
                let Place {
                    id,
                    city: name,
                    capital,
                    lat,
                    lng,
                    country,
                    admin_name,
                    population,
                    ..
                } = place.clone();
                let row = entity::city::ActiveModel {
                    id: Set(id),
                    name: Set(name.clone()),
                    country: Set(country),
                    capital: Set(capital),
                    admin_name: Set(admin_name),
                    population: Set(population),
                    lat: Set(lat),
                    lng: Set(lng),
                };

                // println!("row: {row:#?}");
                let insert = Cities::insert(row);
                // println!("insert: {insert:#?}");
                insert.exec(&db).await.ok();

                // cities.insert(
                //     name.clone(),
                //     City {
                //         id,
                //         country,
                //         capital,
                //         admin_name,
                //         population,
                //         lat,
                //         lng,
                //     },
                // );

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
struct WPPost {
    id: u32,
    date: String,
    slug: String,
    link: String,
    author: u32,
    title: Title,
    content: Html,
    featured_media: u32,
    excerpt: Html,
    categories: Vec<u32>,
    tags: Vec<u32>,
}

#[derive(Debug, Deserialize, Serialize)]
struct Media {
    alt_text: Option<String>,
    mime_type: Option<String>,
    source_url: Option<String>,
}

#[derive(Debug, Deserialize, Serialize, PartialEq, PartialOrd, Eq, Ord, Clone)]
struct Label {
    id: u32,
    name: String,
}

struct EXIF {
    coord: (f32, f32),
    width: u32,
    height: u32,
}

// #[derive(Debug, Deserialize, Serialize, Clone)]
// struct Image {
//     b64: String,
//     width: u32,
//     height: u32,
//     alt: String,
//     coords: Option<Record<String, (f32, f32)>>,
// }

#[derive(Debug, Serialize, Clone)]
struct PostData {
    id: u32,
    date: String,
    title: String,
    slug: String,
    excerpt: String,
    image: Option<Image>,
    link: String,
    categories: BTreeMap<String, u32>,
    tags: BTreeMap<String, u32>,
    coords: Vec<Record<String, (f32, f32)>>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct PostUpdate {
    post_id: u32,
    post: WPPost,

}

#[derive(Clone)]
struct AppState {
    db: Arc<DatabaseConnection>,
    cities: Vec<entity::city::Model>,
    coords: Vec<Coord>,
    post_coords: Vec<Record<String, (f32, f32)>>,
    posts: Vec<Post>,
    menu_items: Vec<MenuItem>,
}

fn distance(p1: &(f32, f32), p2: &(f32, f32)) -> f32 {
    // println!("p1: {p1:?}\t\tp2: {p2:?}");
    f32::sqrt(f32::powf(p2.0 - p1.0, 2f32) + f32::powf(p2.1 - p1.1, 2f32))
}

fn select_coord(
    coords: &BTreeSet<Record<String, (f32, f32)>>,
) -> Option<Record<String, (f32, f32)>> {
    // println!("select_coord coords.len(): {}", coords.len());
    let mut coords = coords.iter().collect::<Vec<&Record<String, (f32, f32)>>>();
    let mut idx: usize = 0;
    while coords.len() > 1 {
        let summ = coords.iter().fold((0.0, 0.0), |accum, coord| {
            (accum.0 + coord.value().0, accum.1 + coord.value().1)
        });
        let avg = (summ.0 / coords.len() as f32, summ.1 / coords.len() as f32);
        let farthest =
            coords
                .iter()
                .enumerate()
                .fold((0, (avg.0, avg.1)), |accum, (idx, coord)| {
                    let a = distance(&avg, &coord.value());
                    let b = distance(&avg, &accum.1);
                    // println!("idx: {idx} a: {a} b: {b}");
                    if a >= b { (idx, *coord.value()) } else { accum }
                });

        coords.remove(farthest.0);
    }

    coords.into_iter().next().cloned()
}

fn cluster(
    coords: &mut Vec<Record<String, (f32, f32)>>,
    rand: &mut random::Xorshift128Plus,
) -> (f32, f32) {
    if coords.is_empty() {
        return (0.0, 0.0);
    }
    if coords.len() == 1 {
        return coords[0].value().clone();
    }
    // eprintln!("r: {}", rand.read::<f32>());
    // let r2 = rand.read::<f32>() / f32::MAX * 180f32;
    let mut iter: random::Sequence<'_, random::Default, f32> = rand.iter();
    iter.next();

    coords.sort_by_key(|rec| rec.value().0 as u32 + rec.value().1 as u32);
    // println!("coords: {coords:?}");
    // println!("coords.len(): {}", coords.len());

    let mut centroid1 = coords[iter.next().unwrap_or(0.0) as usize * coords.len() / 2]
        .value()
        .clone();
    // println!("coords.len(): {}", coords.len());
    let mut centroid2 = coords
        [coords.len() / 2 + iter.next().unwrap_or(0.0) as usize * coords.len() / 2]
        .value()
        .clone();

    let mut cluster1 = Vec::<&(f32, f32)>::new();
    let mut cluster2 = Vec::<&(f32, f32)>::new();

    let mut c1_avg = (180f32, 360f32);
    let mut c2_avg = (180f32, 360f32);

    let mut c1_dist = f32::MAX;
    let mut c2_dist = f32::MAX;

    let mut changes = 1;

    while changes > 0
        && (distance(&centroid1, &c1_avg) > 5f32 || distance(&centroid2, &c2_avg) > 5f32)
    {
        changes = 0;

        coords.iter().for_each(|coord| {
            // println!("coord: {:?}", coord.value());
            // println!("centroid1: {centroid1:?}");
            // println!("centroid2: {centroid2:?}");

            c1_dist = distance(&centroid1, &coord.value());
            c2_dist = distance(&centroid2, &coord.value());

            // println!("c1_dist: {c1_dist}\tc2_dist: {c2_dist}");

            if c1_dist < c2_dist {
                if !cluster1.contains(&&coord.value()) {
                    if cluster2.contains(&&coord.value()) {
                        cluster1.push(
                            cluster2
                                .extract_if(.., |c| *c == coord.value())
                                .next()
                                .unwrap(),
                        );
                        changes += 1;
                    } else {
                        cluster1.push(&coord.value());
                    }
                }
            } else {
                if !cluster2.contains(&&coord.value()) {
                    if cluster1.contains(&&coord.value()) {
                        cluster2.push(
                            cluster1
                                .extract_if(.., |c| *c == coord.value())
                                .next()
                                .unwrap(),
                        );
                        changes += 1;
                    } else {
                        cluster2.push(&coord.value());
                    }
                }
            }

            // println!("cluster1: {cluster1:?}");
            // println!("cluster2: {cluster2:?}");

            let c1_summ = cluster1
                .iter()
                .fold((0f32, 0f32), |accum, c| (accum.0 + c.0, accum.1 + c.1));

            c1_avg = (
                c1_summ.0 / f32::max(cluster1.len() as f32, 0.1e-10),
                c1_summ.1 / f32::max(cluster1.len() as f32, 0.1e-10),
            );

            let c2_summ = cluster2
                .iter()
                .fold((0f32, 0f32), |accum, c| (accum.0 + c.0, accum.1 + c.1));
            c2_avg = (
                c2_summ.0 / f32::max(cluster2.len() as f32, 0.1e-10),
                c2_summ.1 / f32::max(cluster2.len() as f32, 0.1e-10),
            );

            // println!("c1_avg: {c1_avg:?}\tc2_avg: {c2_avg:?}");

            let dy1 = c1_avg.0 - centroid1.0;
            let dx1 = c1_avg.1 - centroid1.1;
            let dy2 = c2_avg.0 - centroid2.0;
            let dx2 = c2_avg.1 - centroid2.1;

            let m1 = dy1 / f32::max(dx1, 0.1e-10);
            let m2 = dy2 / f32::max(dx2, 0.1e-10);

            let rate1 = c1_dist / 2.0 * if dy1 < 0f32 { -1f32 } else { 1f32 };
            let rate2 = c2_dist / 2.0 * if dy2 < 0f32 { -1f32 } else { 1f32 };

            centroid1.0 += m1 * rate1;
            centroid1.1 += rate1;
            centroid2.0 += m2 * rate2;
            centroid2.1 += rate2;
        });
    }
    if cluster1.len() > cluster2.len() {
        centroid1
    } else {
        centroid2
    }
}

async fn extract_exif_from_url(
    url: String,
    client: &reqwest::Client,
) -> Result<Option<EXIF>, Box<dyn Error>> {
    let ext_re = pcre2::bytes::Regex::new(r#"(?:https?://.+/.+\.)(?<ext>.+?)$"#);
    let match_ = ext_re?.captures(url.as_bytes())?.expect("img extension");

    let ext = str::from_utf8(&match_["ext"])?;
    // println!("url: {url}");
    // println!("ext: {ext}");

    let fe = match ext {
        "jpg" | "jpeg" => FileExtension::JPEG,
        "png" => FileExtension::PNG {
            as_zTXt_chunk: false,
        },
        "webp" => FileExtension::WEBP,
        _ => FileExtension::JPEG,
    };

    let ext = match ext {
        "jpg" | "jpeg" => "jpeg",
        other => other,
    };

    let mut mime = format!("image/{ext}").as_str();
    let mut lon: Option<f32> = None;
    let mut lat: Option<f32> = None;

    let res = client.get(url).send().await?;

    // println!("\n{res:?}");
    let mime_header = res.headers().get("content-type").cloned();
    if let Some(hv) = &mime_header {
        mime = hv.to_str()?;
    }
    let bytes = res.bytes().await?;
    // println!("bytes: {bytes:?}");

    let buf = bytes.into_iter().collect::<Vec<u8>>();
    // println!("buf: {buf:?}");
    let mut width = 1480;
    let mut height = 740;

    if let Ok(metadata) = Metadata::new_from_vec(&buf, fe) {
        // println!("metadata: {metadata:?}");

        if let Some(ExifTag::GPSLongitude(lon_)) = &mut metadata
            .get_tag(&ExifTag::GPSLongitude(RATIONAL64U::new()))
            .next()
        {
            if let Some(ExifTag::GPSLongitudeRef(lon_ref)) = metadata
                .get_tag(&ExifTag::GPSLongitudeRef("".to_string()))
                .next()
            {
                // println!("lon_ref: {lon_ref}");
                let m = if *lon_ref == "E".to_string() {
                    1f32
                } else {
                    -1f32
                };
                // println!("lon: {lon_:?}");
                lon = Some(lon_[0].nominator as f32 * m);
            }
        }

        if let Some(ExifTag::GPSLatitude(lat_)) = &mut metadata
            .get_tag(&ExifTag::GPSLatitude(RATIONAL64U::new()))
            .next()
        {
            if let Some(ExifTag::GPSLatitudeRef(lat_ref)) = metadata
                .get_tag(&ExifTag::GPSLatitudeRef("".to_string()))
                .next()
            {
                // println!("lat_ref: {lat_ref}");
                let m = if *lat_ref == "N".to_string() {
                    1f32
                } else {
                    -1f32
                };
                // println!("lat: {lat_:?}");
                lat = Some(lat_[0].nominator as f32 * m);
            }
        }

        if let Some(ExifTag::ImageWidth(w)) =
            metadata.get_tag(&ExifTag::ImageWidth(INT32U::new())).next()
        {
            width = w[0];
        }
        if let Some(ExifTag::ImageHeight(h)) = metadata
            .get_tag(&ExifTag::ImageHeight(INT32U::new()))
            .next()
        {
            height = h[0];
        }
    }
    if let Some(lt) = lat
        && let Some(ln) = lon
    {
        Ok(Some(EXIF {
            coord: (lt, ln),
            width,
            height,
        }))
    } else {
        Ok(None)
    }
}

async fn fetch_img_meta(id: u32, client: &reqwest::Client) -> Result<Media, reqwest::Error> {
    let res = client
        .get(format!(
            "https://ozimage.com.au/wp-json/wp/v2/media/{id}?_fields=alt_text,mime_type,source_url"
        ))
        .send()
        .await?;

    res.json::<Media>().await
}

async fn fetch_img(
    id: u32,
    client: &reqwest::Client,
) -> Result<Option<(Bytes, Option<String>)>, reqwest::Error> {
    let media = fetch_img_meta(id, &client).await?;
    if let Some(url) = media.source_url {
        let res = client.get(url).send().await?;
        Ok(Some((res.bytes().await?, media.mime_type)))
    } else {
        Ok(None)
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    from_filename(".env.local").ok();
    let db = Database::connect("sqlite://db/db.sqlite?mode=rwc").await?;
    let schema_manager = SchemaManager::new(&db);

    db.get_schema_registry("places-ai::entity::*")
        .sync(&db)
        .await?;

    let state = Arc::<AppState>::new(AppState {
        db: Arc::new(db.clone()),
        cities: Vec::new(),
        coords: Vec::new(),
        post_coords: Vec::new(),
        posts: Vec::new(),
        menu_items: Vec::new(),
    });

    print!("Initializing server...");

    let cors = CorsLayer::new()
        .allow_methods([http::Method::GET])
        .allow_origin(Any);

    tracing_subscriber::fmt::init();

    let app = Router::<Arc<AppState>>::new()
        .route("/menu/items", get(get_menu_items))
        .route("/posts", get(get_posts))
        .route("/posts/ids", get(get_post_ids))
        .route("/post/data/{id}", get(get_post_data))
        .route("/post/{slug}", get(get_post))
        .route("/posts/coords", get(get_coords))
        .route("/image/{id}", get(get_image))
        .route("/posts/featured", get(get_featured_posts))
        .route("/images", get(get_feature_images))
        .route("/ping", get(ping))
        .with_state(state)
        .route_layer(cors);

    let listener = TcpListener::bind("0.0.0.0:8080").await?;
    tracing::debug!("listening on {}", listener.local_addr().unwrap());
    let server = tokio::spawn(async move {
        if let Err(err) = serve(listener, app).await {
            eprintln!("server error: {err}");
        }
    });

    let client = reqwest::Client::new();
    let username = std::env::var("USERNAME")?;
    let password = std::env::var("PASSWORD")?;
    let b64 = format!("{username}:{password}")
        .as_bytes()
        .to_base64(STANDARD);

    if MenuItems::find().one(&db).await?.is_none() {
        let menu_items: Vec<MenuItem> = {
            let res = client
        .get(format!("https://ozimage.com.au/wp-json/wp/v2/menu-items?_fields=id,title,parent,menu_order,url"))
        .header("Authorization", format!("Basic {b64}"))
        .send()
        .await?;
            println!("res: {res:?}");

            let menu_items = res.json().await?;
            println!("menu_items: {menu_items:?}");

            menu_items
        };
        for item in menu_items {
            println!("item: {:?}", item);
            MenuItems::insert(menu_item::ActiveModel {
                id: Set(item.id),
                title: Set(item.title),
                parent: Set(item.parent),
                menu_order: Set(item.menu_order),
                url: Set(item.url),
            }).exec(&db).await?;
        }
    }

    let menu_items = MenuItems::find().all(&db).await?;

    // println!("menu_items: {menu_items:#?}");

    // let mut cities = BTreeMap::<String, City>::new();
    let mut cities = Vec::<City>::new();
    let mut coords = Vec::<Coord>::new();

    if Cities::find().one(&db).await?.is_none() {
        (cities, coords) = parse_csv(db.clone()).await?;
    } else {
        let res = Cities::find();
        println!("{}", res.build(DbBackend::Sqlite).to_string());
        cities = res.all(&db).await?;
    }

    let mut post_coords = Vec::<Record<String, (f32, f32)>>::new();
    let mut posts = Vec::<Post>::new();
    if Posts::find().one(&db).await?.is_none() {
        print!("Fetching post data...");

        let wp_posts =
    client.get("https://ozimage.com.au/wp-json/wp/v2/posts?_fields=date,author,id,title,slug,excerpt,content,featured_media,link,categories,tags&per_page=100").send().await?.json::<Vec<WPPost>>().await?;
        println!("done");
        // println!("{posts:?}");
        print!("Fetching categories...");
        let categories = client
            .get("https://ozimage.com.au/wp-json/wp/v2/categories?_fields=id,name&per_page=100")
            .send()
            .await?
            .json::<Vec<Label>>()
            .await?;
        println!("done");
        print!("Fetching tags...");
        let tags = client
            .get("https://ozimage.com.au/wp-json/wp/v2/tags?_fields=id,name")
            .send()
            .await?
            .json::<Vec<Label>>()
            .await?;
        println!("done");

        println!("Caching post data...");
        let mut post_data = Vec::<PostData>::new();
        // let re = Regex::new(r"(\p{Lu}\p{Ll}+)(\s\p{Lu}\p{Ll}*)*")?;
        let re = Regex::new(r"\b(\p{Lu}\p{Ll}{2,})(\s\p{Lu}\p{Ll}*)*")?;

        // <img decoding="async" class="rsImg rsMainSlideImage lazyloaded" data-src="https://ozimage.com.au/wp-content/uploads/Gallery-and-wine-tasting-room-2.jpg" alt="" src="https://ozimage.com.au/wp-content/uploads/Gallery-and-wine-tasting-room-2.jpg" style="--smush-placeholder-width: 1095px; --smush-placeholder-aspect-ratio: 1095/602; width: 805px; height: 443px;">

        let img_re = Regex::new(
            r#"<img.*?(width="(?<width>\d+?)".*?height="(?<height>\d+?)".*?)?(?:data-)?src="(?<url>http.+/(?<filename>.+\.(?<ext>png|jpg|jpeg)))".*?alt="(?<alt>.*?)?".*?/?>"#,
        )?;
        let mut default_city_name = String::new();
        let mut city_names = Vec::<String>::new();
        // println!("{:?}", posts[0].content.rendered.as_bytes());
        for post in &wp_posts {
            let WPPost {
                id,
                title,
                date,
                slug,
                author,
                excerpt,
                content,
                featured_media,
                link,
                categories: cat_list,
                tags: tag_list,
                ..
            } = post.clone();

            let title = title.rendered;
            // println!("title: {title}");

            // print!("\t ...extracting main image...");
            let mut b64 = String::new();
            let mut width: u32 = 1480;
            let mut height: u32 = 740;
            let mut alt = "".to_string();
            let mut place_name = "".to_string();
            let mut img_coords: Option<Record<String, (f32, f32)>> = None;

            let mut image: Option<Image> = None;

            let excerpt = excerpt.rendered;
            let cat_data = BTreeMap::<String, u32>::from_iter(
                categories
                    .iter()
                    .map(|cat| (cat.name.clone(), cat.id.clone()))
                    .filter(|(_, v)| cat_list.contains(v)),
            );
            let tag_data = BTreeMap::<String, u32>::from_iter(
                tags.iter()
                    .map(|tag| (tag.name.clone(), tag.id.clone()))
                    .filter(|(_, v)| tag_list.contains(v)),
            );

            let mut coord_set = BTreeSet::<Record<String, (f32, f32)>>::new();
            let mut coords = Vec::<Record<String, (f32, f32)>>::new();

            let mut countries = BTreeSet::<String>::new();
            let mut admin_names = BTreeSet::<String>::new();
            let mut capitals = BTreeSet::<String>::new();

            let search_subject = cat_data
                .keys()
                .cloned()
                .collect::<Vec<String>>()
                .join(" ")
                .chars()
                .chain(post.title.rendered.chars())
                .chain(post.excerpt.rendered.chars())
                .chain(post.content.rendered.chars())
                .collect::<String>();

            // println!("search_subject: {search_subject}");

            let mut counter = 0;
            for result in re.find_iter(search_subject.as_bytes()) {
                counter += 1;
                let match_ = result?;
                let match_string = String::from_utf8(match_.as_bytes().to_vec())
                    .unwrap_or("Failed to match".to_string());
                // eprintln!("match_string: {match_string:?}");
                // for i in 0..(caps.len()) {
                //     // println!("i: {i}\t\tcaps.len(): {}", caps.len());
                //     if let Ok(match_) = str::from_utf8(&caps[i])

                //     {
                //         println!("match_: {match_}");
                if let Some(city) = cities.iter().find(|c| {
                    // println!("n: {n}");
                    match &match_string {
                        admin if *match_string == c.admin_name => {
                            // println!("admin_name: {admin}");
                            admin_names.insert(admin.clone());
                            true
                        }
                        country if *match_string == c.country => {
                            // println!("country: {country}");
                            countries.insert(country.clone());
                            true
                        }
                        name if *match_string == c.name => {
                            // println!("city_name: {name}");
                            city_names.push(name.clone());
                            true
                        }
                        other => {
                            // println!("other: {other}");
                            false
                        }
                    }
                }) {
                    // println!("name: {name}");
                    let coord = Record::new(match_string, (city.lat, city.lng));
                    coord_set.insert(coord.clone());
                    post_coords.push(coord);
                    place_name = city.name.clone();
                    // println!("place_name: {place_name}");
                }
                // }
                // }
            }
            // println!("counter: {counter}");
            let mut captures_iter = img_re.captures_iter(content.rendered.as_bytes());
            // println!("captures_iter.count(): {}", &&captures_iter.count());
            while let Some(Ok(cap)) = captures_iter.next() {
                // println!("\ncaptures: {:?}", &captures);
                let url = str::from_utf8(&cap["url"])?;
                // println!("url: {url}");
                if let Some(a) = cap.name("alt") {
                    alt = str::from_utf8(&a.as_bytes())?.to_string();
                }
                if let Some(w) = cap.name("width") {
                    if let Ok(n) = u32::from_str_radix(str::from_utf8(&w.as_bytes())?, 10) {
                        width = n;
                    }
                }
                if let Some(h) = cap.name("height") {
                    if let Ok(n) = u32::from_str_radix(str::from_utf8(&h.as_bytes())?, 10) {
                        height = n;
                    }
                }

                // println!("city_names.len(): {}", city_names.len());

                default_city_name = city_names.iter().fold("".to_string(), |n1, n2| {
                    if city_names.iter().filter(|n| **n == *n2).count()
                        > city_names.iter().filter(|n| **n == n1).count()
                    {
                        n2.clone()
                    } else {
                        n1
                    }
                });

                if !default_city_name.is_empty() {
                    // println!("default_city_name: {default_city_name}");
                }

                if let Some(exif) = extract_exif_from_url(url.to_string(), &client).await? {
                    coord_set.insert(Record::<String, (f32, f32)>::new(
                        default_city_name.clone(),
                        exif.coord,
                    ));
                }
            }
            println!("fetching featured_media {featured_media}");
            // let res =  client.get(format!(
            //     "https://ozimage.com.au/wp-json/wp/v2/media/{featured_media}?_fields=alt_text,mime_type,source_url"
            // )).send().await?;

            // let media = res.json::<Media>().await?;

            // let res = client.get(media.source_url).send().await?;

            if let Some((img_bytes, Some(mime_type))) = fetch_img(featured_media, &client).await? {
                print!("\t...converting to base64...");
                b64.push_str(format!("data:{};base64,", mime_type).as_str());

                b64.push_str(&img_bytes.to_base64(STANDARD));
                if b64 == "" {
                    println!("data for image with alt {alt:?} missing");
                }
                println!("done");
            }
            let meta = fetch_img_meta(featured_media, &client).await?;
            // println!("meta: {meta:?}");
            if let Some(url) = meta.source_url {
                if let Some(exif) = extract_exif_from_url(url, &client).await? {
                    width = exif.width;
                    height = exif.height;
                    let rec =
                        Record::<String, (f32, f32)>::new(default_city_name.clone(), exif.coord);
                    default_city_name.clear();
                    img_coords = Some(rec.clone());
                    coord_set.insert(rec);
                }
            }

            // if !(lt.is_nan() || lt.is_infinite() || ln.is_nan() || ln.is_infinite()) {
            //     let img_record =
            //         Record::<String, (f32, f32)>::new(name, (lt, ln));
            //     img_coords = Some(img_record.clone());
            //     coord_set.insert(img_record);
            // }

            // println!("coord_set.len(): {}", coord_set.len());
            //
            let pin_coord = select_coord(
                &mut coord_set, // .into_iter()
                                // .collect::<Vec<Record<String, (f32, f32)>>>(),
                                // &mut rand,
            );
            // println!("pin_coord: {pin_coord:?}");
            if let Some(coord) = pin_coord {
                coords.clear();
                coords.push(coord);
            }
            // coords = std::mem::take(&mut coord_set.into_iter().collect::<Vec<Record<String, (f32, f32)>>>());
            // if let Some(coord) = pin_coord {
            // coords.insert(0, Record::<String, (f32, f32)>::new(place_name, coord));
            // }

            image = Some(Image {
                id: featured_media,
                b64,
                width,
                height,
                alt,
                coords: JsonField::<Option<Record<String, (f32, f32)>>>(img_coords),
            });

            city_names.clear();

            let row = entity::post::ActiveModel {
                id: Set(id),
                date: Set(date),
                slug: Set(slug),
                link: Set(link),
                author: Set(author),
                title: Set(title),
                content: Set(content.rendered),
                image: Set(JsonField::<Image>(image.expect("empty string"))),
                excerpt: Set(excerpt),
                categories: Set(JsonField::<BTreeMap<String, u32>>(cat_data)),
                tags: Set(JsonField::<BTreeMap<String, u32>>(tag_data)),
                coords: Set(JsonField::<Option<Vec<Record<String, (f32, f32)>>>>(Some(
                    coords,
                ))),
            };
            let res = Posts::insert(row);
            // println!("{}", res.build(DbBackend::Sqlite).to_string());
            res.exec(&db).await.ok();
        }

        let slug_map = Arc::new(BTreeMap::<String, Html>::from_iter(
            wp_posts.iter().map(|d| (d.slug.clone(), d.content.clone())),
        ));

        let ids = post_data.iter().map(|d| d.id).collect::<Vec<u32>>();
        post_data.sort_by(|d1, d2| d1.id.cmp(&d2.id));
    } else {
        let res = Posts::find();
        println!("{}", res.build(DbBackend::Sqlite).to_string());
        posts = res.all(&db).await?
    }

    println!("done");
    server.await?;
    println!("finished");
    Ok(())
}

#[debug_handler]
async fn get_posts(State(state): State<Arc<AppState>>) -> Json<Vec<Post>> {
    let mut data = Posts::find()
        // .select_only()
        .columns([
            post::Column::Id,
            post::Column::Date,
            post::Column::Title,
            post::Column::Slug,
            post::Column::Excerpt,
            post::Column::Image,
            post::Column::Link,
            post::Column::Categories,
            post::Column::Tags,
            post::Column::Coords,
        ])
        .all(&*state.db)
        .await
        .ok()
        .unwrap_or_default();
    // println!("data: {data:?}");
    data.sort_by(|p1, p2| {
        let d1 = chrono::NaiveDateTime::parse_from_str(p1.date.as_str(), "%Y-%m-%dT%H:%M:%S")
            .ok()
            .unwrap_or_default();
        let d2 = chrono::NaiveDateTime::parse_from_str(p2.date.as_str(), "%Y-%m-%dT%H:%M:%S")
            .ok()
            .unwrap_or_default();
        d2.cmp(&d1)
    });
    // println!("data: {data:?}");
    let value = Json(data);
    
    value
}

#[debug_handler]
async fn get_post_ids(State(state): State<Arc<AppState>>) -> Json<Vec<u32>> {
    let mut ids = Posts::find()
        // .select_only()
        .column(post::Column::Id)
        .all(&*state.db)
        .await
        // .ok()
        .unwrap_or_default();
    ids.sort_by(|p1, p2| {
        let d1 = chrono::NaiveDateTime::parse_from_str(p1.date.as_str(), "%Y-%m-%dT%H:%M:%S")
            .ok()
            .unwrap_or_default();
        let d2 = chrono::NaiveDateTime::parse_from_str(p2.date.as_str(), "%Y-%m-%dT%H:%M:%S")
            .ok()
            .unwrap_or_default();
        d2.cmp(&d1)
    });
    Json(
        ids.iter()
            .skip(7)
            .filter(|post| {
                if post.image.0.b64.as_str() != "" {
                    true
                } else {
                    println!("post with title {} has no feature image", post.title);
                    false
                }
            })
            .map(|post| post.id)
            .collect::<Vec<u32>>(),
    )
}

#[debug_handler]
async fn get_post(State(state): State<Arc<AppState>>, Path(slug): Path<String>) -> Json<String> {
    let post = Posts::find()
        .select_only()
        .filter(post::Column::Slug.eq(slug))
        .column(post::Column::Content)
        .one(&*state.db)
        .await
        .ok()
        .unwrap_or_default()
        .unwrap_or_default();
    // let post = state
    //     .slug_map
    //     .get(&slug)
    //     .unwrap_or(&Html {
    //         rendered: "".to_string(),
    //         protected: true,
    //     })
    //     .clone();

    Json(post.content)
}

#[debug_handler]
async fn get_post_data(State(state): State<Arc<AppState>>, Path(id): Path<u32>) -> Json<Post> {
    let res = Posts::find_by_id(id)
        // .select_only()
        .columns([
            post::Column::Id,
            post::Column::Date,
            post::Column::Title,
            post::Column::Slug,
            post::Column::Excerpt,
            post::Column::Image,
            post::Column::Link,
            post::Column::Categories,
            post::Column::Tags,
            post::Column::Coords,
        ]);

    // println!("{}", res.build(DbBackend::Sqlite).to_string());
    let mut value: Json<post::Model>;
    let query = res.one(&*state.db).await.ok();
    if let Some(Some(data)) = query {
        value = Json(data);
    } else {
        value = Json(post::Model::default());
    }
    value
}

#[debug_handler]
async fn get_coords(State(state): State<Arc<AppState>>) -> Json<Vec<Record<String, (f32, f32)>>> {
    let posts = Posts::find()
        .select_only()
        .column(post::Column::Coords)
        .all(&*state.db)
        .await
        .unwrap_or_default();

    let mut coords = Vec::new();
    for post in posts {
        if let Some(values) = post.coords.0.clone() {
            coords.extend(values);
        }
    }

    Json(coords)
}

#[debug_handler]
async fn get_feature_images(State(state): State<Arc<AppState>>) -> Json<Vec<Image>> {
    println!("get_featured_images");
    let res = Posts::find()
        .order_by_desc(entity::post::Column::Date)
        .column(entity::post::Column::Image);
    println!("{}", res.build(DbBackend::Sqlite).to_string());
    let res = res.all(&*state.db).await.ok();
    if let Some(images) = res {
        // println!("images: {images:?}");
        Json(
            images
                .iter()
                .take(7)
                .map(|img| img.image.0.clone())
                .collect::<Vec<Image>>(),
        )
    } else {
        Json(Vec::<Image>::new())
    }
}

#[debug_handler]
async fn get_featured_posts(State(state): State<Arc<AppState>>) -> Json<Vec<Post>> {
    println!("get_featured_posts");
    let res = Posts::find().order_by_desc(entity::post::Column::Date);
    let res = res.all(&*state.db).await.ok();
    if let Some(mut posts) = res.to_owned() {
        println!("posts[0].date: {:?}", posts[0].date);
        posts.sort_by(|p1, p2| {
            // println!("p1.date: {}", p1.date.as_str());
            // YYYY-MM-DDTHH:MM:SS
            let d1 = chrono::NaiveDateTime::parse_from_str(p1.date.as_str(), "%Y-%m-%dT%H:%M:%S")
                .ok()
                .unwrap_or_default();
            let d2 = chrono::NaiveDateTime::parse_from_str(p2.date.as_str(), "%Y-%m-%dT%H:%M:%S")
                .ok()
                .unwrap_or_default();
            d2.cmp(&d1)
        });

        Json(posts.into_iter().take(7).collect::<Vec<Post>>())
    } else {
        Json(Vec::<Post>::new())
    }
}

#[debug_handler]
async fn get_image(State(state): State<Arc<AppState>>, Path(id): Path<u32>) -> Json<Image> {
    let res = Posts::find()
        .column(post::Column::Image)
        .all(&*state.db)
        .await
        .ok();

    if let Some(images) = res {
        if let Some(img) = images.into_iter().find(|i| i.id == id) {
            Json(img.image.clone().0)
        } else {
            Json(Image::default())
        }
    } else {
        Json(Image::default())
    }
}

#[debug_handler]
async fn get_menu_items(State(state): State<Arc<AppState>>) -> Json<Vec<MenuItem>> {
    let menu_items = MenuItems::find().order_by_asc(menu_item::Column::MenuOrder).all(&*state.db).await.unwrap_or_default();
    println!("menu_items: {menu_items:?}");
    Json(MenuItems::find().order_by_asc(menu_item::Column::MenuOrder).all(&*state.db).await.unwrap_or_default())
}

async fn ping() -> String {
    "pong".to_string()
}

// #[debug_handler]
// async fn post_update_post() {
    
// }
