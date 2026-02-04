mod entity;

use axum::{
    Json, Router,
    extract::{DefaultBodyLimit, Path, Query, State},
    http::{self},
    routing::get,
    serve,
};
use axum_macros::debug_handler;
use bytes::Bytes;
use chrono::{DateTime, Utc};
use csv::ReaderBuilder;
// use regex::Regex;
use b64_ct::{STANDARD, ToBase64};
use pcre2::bytes::Regex;
use serde::{Deserialize, Serialize};

use sea_orm::{
    ActiveValue::Set,
    ColumnTrait, ConnectOptions, Database, DatabaseConnection,
    EntityTrait, FromQueryResult, JsonField, QueryFilter, QueryOrder, QuerySelect, QueryTrait,
};

use entity::{
    city::Entity as Cities, image::Image, menu_item::Entity as MenuItems,
    menu_item::Model as MenuItem, post::Entity as Posts, post::Model as Post, tp5d::{Entity as Tp5ds, Model as TP5D}, 
};
use serde_json::Value;

use std::{ cmp::{Ordering, PartialOrd}, collections::{BTreeMap, BTreeSet}, error::Error, io::Read, ops::{Deref, DerefMut }, sync::{Arc, Mutex}, time::Duration
};
use tokio::net::TcpListener;
use tower_http::cors::{Any, CorsLayer};

use log::info;

use entity::record::Record;

use dotenv::from_filename;

use random;
use random::Source;

use crate::entity::{menu_item, post};

// const DATABASE_URL: &str = "sqlite::memory:";
const DB_NAME: &str = "ozimage";
const DEFAULT_POSTS_PER_PAGE: u64 = 24;
const MAX_POSTS_PER_PAGE: u64 = 50;

// async fn open_db() -> Result<DatabaseConnection, DbErr> {
//     let db = SqlxSqliteConnector::connect(ConnectOptions::new(DATABASE_URL)).await?;
//     Ok(db)
// }

#[derive(Debug, Deserialize, Clone)]
struct Place {
    pub id: u64,
    pub city: String,
    pub city_ascii: String,
    pub lat: f32,
    pub lng: f32,
    pub country: String,
    pub iso2: String,
    pub iso3: String,
    pub admin_name: String,
    pub capital: String,
    pub population: u64,
}

// #[derive(Debug, Serialize, Clone)]
// struct City {
// id: u64,
// name: String,
// country: String,
// capital: String,
// admin_name: String,
// population: u64,
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
//     id: u64,
//     title: Record<String, String>,
//     status: String,
//     url: String,
//     attr_title: String,
//     description: String,
//     parent: u64,
//     menu_order: u64,
//     target: String,
//     classes: Vec<String>,
//     _links:

// }

async fn parse_csv(db: DatabaseConnection) -> Result<Vec<entity::city::Model>, Box<dyn Error>> {
    let mut cities = Vec::<entity::city::Model>::new();
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
                } = place;
                let row = entity::city::ActiveModel {
                    id: Set(id as u32),
                    name: Set(name.clone()),
                    country: Set(country.clone()),
                    capital: Set(capital.clone()),
                    admin_name: Set(admin_name.clone()),
                    population: Set(population as u32),
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

                cities.push(entity::city::Model {
                    id: id as u32,
                    name,
                    country,
                    capital,
                    admin_name,
                    population: population as u32,
                    lat,
                    lng,
                });
            }
            Err(e) => {
                // println!("{e:?}");
                // return Err(Box::new(e));
                continue;
            }
        }
    }

    Ok(cities)
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct Menu {
        pub id: u32,
        pub slug: String,
    }

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct WPMenuItem {
    pub id: u32,
    pub title: menu_item::Html,
    pub parent: u32,
    pub menus: u32,
    pub menu_order: u32,
    pub url: String,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
struct MenuMeta {
    id: u64,
    slug: String,
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
    id: u64,
    date: String,
    slug: String,
    link: String,
    author: u64,
    title: Title,
    content: Html,
    featured_media: u64,
    excerpt: Html,
    meta: Value,
    categories: Vec<u64>,
    tags: Vec<u64>,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
struct Media {
    alt_text: Option<String>,
    mime_type: Option<String>,
    source_url: Option<String>,
}

impl Default for Media {
    fn default() -> Self {
        Self {
            alt_text: Some(String::new()),
            mime_type: Some(String::new()),
            source_url: Some(String::new())
        }
    }
}

#[derive(Debug, Deserialize, Serialize, PartialEq, PartialOrd, Eq, Ord, Clone)]
struct Label {
    id: u64,
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
//     width: u64,
//     height: u64,
//     alt: String,
//     coords: Option<Record<String, (f32, f32)>>,
// }

#[derive(Debug, Serialize, Clone)]
struct PostData {
    id: u64,
    date: String,
    title: String,
    slug: String,
    excerpt: String,
    image: Option<Image>,
    link: String,
    categories: BTreeMap<String, u64>,
    tags: BTreeMap<String, u64>,
    coords: Vec<Record<String, (f32, f32)>>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct PostUpdate {
    post_id: u64,
    post: WPPost,

}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct Tp5d {
    fg: Vec<u8>,
    bg: Vec<u8>
}

#[derive(Debug, Deserialize)]
struct PostsQuery {
    page: Option<u64>,
    per_page: Option<u64>,
}

#[derive(Debug, Serialize, FromQueryResult)]
struct PostListItem {
    id: u32,
    date: String,
    title: String,
    slug: String,
    excerpt: String,
    image: JsonField<Image>,
    fg: String,
    bg: String,
    link: String,
    continent: String,
    categories: JsonField<BTreeMap<String, u32>>,
    tags: JsonField<BTreeMap<String, u32>>,
    coords: JsonField<Option<Vec<Record<String, (f32, f32)>>>>,
}

#[derive(Debug, FromQueryResult)]
struct PostIdRow {
    id: u32,
    date: String,
    title: String,
    image: JsonField<Image>,
}

#[derive(Debug, FromQueryResult)]
struct ImageRow {
    image: JsonField<Image>,
}

#[derive(Clone)]
struct AppState {
    db: Arc<DatabaseConnection>,
    last_update: Arc<Mutex<DateTime<Utc>>>
}

// impl<T> Deref for AppState {
//     type Target = T;
//     fn deref(&self) -> &Self::Target {
        
//     }
// }

fn distance(p1: &(f32, f32), p2: &(f32, f32)) -> f32 {
    // println!("p1: {p1:?}\t\tp2: {p2:?}");
    f32::sqrt(f32::powf(p2.0 - p1.0, 2f32) + f32::powf(p2.1 - p1.1, 2f32))
}

async fn fetch_img_meta(id: u64, client: &reqwest::Client, username: String, password: String) -> Result<Media, reqwest::Error> {
    let url = format!("https://ozimage.com.au/wp-json/wp/v2/media/{id}?_fields=alt_text,mime_type,source_url");
    let b64 = format!("{username}:{password}")
        .as_bytes()
        .to_base64(STANDARD);
    println!("fetch_img_meta: {url}");
    let res = client
        .get(url.as_str())
        .header("User-Agent", "curl/8.0.0")
        .header("Authorization", format!("Basic {b64}"))
        .send().await?;
        // println!("{:?}", &mut res.text().await?);
        let json =res.json::<Media>().await?;
    info!("json: {:?}", json);
    Ok(json)
}

async fn fetch_img(
    id: u64,
    client: &reqwest::Client,
    username: String,
    password: String
) -> Result<Option<(Bytes, Option<String>)>, reqwest::Error> {
    println!("fetch_img");
    let media = fetch_img_meta(id, &client, username, password).await?;
    // println!("media: {:?}", media.clone());
    if let Some(url) = media.source_url {
        println!("url: {:?}", url.clone());
        let res = client.get(url).send().await?;
        // println!("res: {res:?}");
        Ok(Some((res.bytes().await?, media.mime_type)))
    } else {
        Ok(None)
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    #[cfg(debug_assertions)]
    from_filename(".env.local").ok();
    
    let wp_server = std::env::var("WP_SERVER")?;

    println!("TP5D_SERVER: {:?}", std::env::var("TP5D_SERVER")?);

    println!("initialising tracing_subscriber");
    tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .with_ansi(false) // Disable colors for cleaner Fly.io logs
        .init();
    
    // let subscriber = tracing_subscriber::fmt().log_internal_errors(true).with_line_number(true).finish();
    
    let username = std::env::var("USERNAME")?;
    let password = std::env::var("PASSWORD")?;
    let b64 = format!("{username}:{password}")
        .as_bytes()
        .to_base64(STANDARD);
    let opt = ConnectOptions::new("sqlite://db/db.sqlite?mode=rwc")
        .min_connections(1)
        .max_connections(5)
        .connect_timeout(Duration::from_secs(8))
        .idle_timeout(Duration::from_secs(8))
        .acquire_timeout(Duration::from_secs(8))
        .sqlx_logging(true)
        .to_owned();
    log::info!("connecting to db");
    let db = Database::connect(opt).await.map_err(|e| {
        // eprintln!("{e:?}");
        log::error!("DbErr: {e:?}");
        log::debug!("{e:?}");
        // error!("{e:?}");
        e
    })?;
    db.get_schema_registry("places-ai::entity::*")
        .sync(&db)
        .await?;

    let state = AppState {
        db: Arc::new(db.clone()),
        last_update: Arc::new(Mutex::new(Utc::now()))
    };

    info!("Initializing server...");

    let cors = CorsLayer::new()
        .allow_methods([http::Method::GET, http::Method::POST])
        .allow_origin(Any);

    let app = Router::<Arc<AppState>>::new()
        .layer(DefaultBodyLimit::max(8_388_608))
        .route("/menu/items", get(get_menu_items))
        .route("/posts", get(get_posts))
        .route("/posts/ids", get(get_post_ids))
        .route("/post/data/{id}", get(get_post_data))
        .route("/post/{slug}", get(get_post))
        .route("/coords", get(get_coords))
        .route("/image/{id}", get(get_image))
        .route("/posts/featured", get(get_featured_posts))
        .route("/images", get(get_feature_images))
        .route("/tp5d/{id}", get(get_tp5d))
        .route("/last_update", get(last_update))
        .route("/ping", get(ping))
        .with_state(Arc::new(state))
        .route_layer(cors);
        // .layer(TraceLayer::new_for_http());
       
        

    let listener = TcpListener::bind("0.0.0.0:8080").await?;
    info!("listening on {}", listener.local_addr().unwrap());
    let server = tokio::spawn(async move {
        if let Err(err) = serve(listener, app).await {
            eprintln!("server error: {err}");
        }
    });

    let client = reqwest::Client::new();
    
    if MenuItems::find().one(&db).await?.is_none() {
        info!("fetching main-menu id...");
        let menu_id: u32 = client
            .get(format!("{wp_server}/menus?_fields=id,slug&search=main-menu&search-columns=slug"))
            .header("Authorization", format!("Basic {b64}"))
            .send()
            .await?
            .json::<Vec<Menu>>()
            .await?
            .get(0)
            .map(|menu| menu.id)
            .unwrap_or_default();
        info!("menu_id: {}", menu_id);
        info!("fetching menu items...");
        let menu_items: Vec<WPMenuItem> = {
            let res = client
        .get(format!("https://ozimage.com.au/wp-json/wp/v2/menu-items?_fields=id,title,parent,menus,menu_order,url"))
        .header("Authorization", format!("Basic {b64}"))
        .send()
        .await?;
            // println!("res: {res:?}");

            let menu_items = res.json().await?;
            // println!("menu_items: {menu_items:?}");

            menu_items
        };
        for item in menu_items {
            // println!("item: {:?}", item);
            if item.menus == menu_id {
            MenuItems::insert(menu_item::ActiveModel {
                id: Set(item.id),
                title: Set(JsonField::<menu_item::Html>(item.title)),
                parent: Set(item.parent),
                menu_order: Set(item.menu_order),
                url: Set(item.url),
            }).exec(&db).await?;
        }
        }
    }

    if Posts::find().one(&db).await?.is_none() {
        info!("Fetching post data...");
        let cities = if Cities::find().one(&db).await?.is_none() {
            parse_csv(db.clone()).await?
        } else {
            Cities::find().all(&db).await?
        };

        let wp_posts =
    client.get(format!("{wp_server}/posts?_fields=date,author,id,title,slug,excerpt,content,featured_media,link,categories,tags,meta&per_page=100").as_str()).send().await?.json::<Vec<WPPost>>().await?;
        println!("done");
        // println!("{posts:?}");
        print!("Fetching categories...");
        let categories = client
            .get(format!("{wp_server}/categories?_fields=id,name&per_page=100"))
            .send()
            .await?
            .json::<Vec<Label>>()
            .await?;

        println!("done");
        print!("Fetching tags...");
        let tags = client
            .get(format!("{wp_server}/tags?_fields=id,name"))
            .send()
            .await?
            .json::<Vec<Label>>()
            .await?;
        println!("done");

        println!("Caching post data...");
        // let re = Regex::new(r"(\p{Lu}\p{Ll}+)(\s\p{Lu}\p{Ll}*)*")?;
        let re = Regex::new(r"\b(\p{Lu}\p{Ll}{2,})(\s\p{Lu}\p{Ll}*)*")?;

        // <img decoding="async" class="rsImg rsMainSlideImage lazyloaded" data-src="https://ozimage.com.au/wp-content/uploads/Gallery-and-wine-tasting-room-2.jpg" alt="" src="https://ozimage.com.au/wp-content/uploads/Gallery-and-wine-tasting-room-2.jpg" style="--smush-placeholder-width: 1095px; --smush-placeholder-aspect-ratio: 1095/602; width: 805px; height: 443px;">

        let img_re = Regex::new(
            r#"<img.*?(width="(?<width>\d+?)".*?height="(?<height>\d+?)".*?)?(?:data-)?src="(?<url>http.+/(?<filename>.+\.(?<ext>png|jpg|jpeg)))".*?alt="(?<alt>.*?)?".*?/?>"#,
        )?;
        for post in &wp_posts {
            let id = post.id;
            let title = post.title.rendered.clone();
            let date = post.date.clone();
            let slug = post.slug.clone();
            let author = post.author;
            let excerpt = post.excerpt.rendered.clone();
            let content_rendered = post.content.rendered.clone();
            let featured_media = post.featured_media;
            let link = post.link.clone();
            let cat_list = &post.categories;
            let tag_list = &post.tags;
            let meta = &post.meta;
            // println!("title: {title}");

            // print!("\t ...extracting main image...");
            let mut b64 = String::new();
            let mut width: u32 = 1480;
            let mut height: u32 = 740;
            let mut alt = "".to_string();
            let img_coords: Option<Record<String, (f32, f32)>> = None;

            let mut image: Option<Image> = None;

            let cat_data = BTreeMap::<String, u32>::from_iter(
                categories
                    .iter()
                    .map(|cat| (cat.name.clone(), cat.id.clone() as u32))
                    .filter(|(_, v)| cat_list.contains(&(*v as u64))),
            );

            let continent = if let Value::Object(map) = meta {
                let name = if let Value::Number(cat) = &map["wds_primary_category"] {
                    if let Some((name, _)) = cat_data.iter().find(|(_, id)| cat.as_u64() == Some((**id).into())) {
                        name.clone()
                    } else if let Some((name, _)) = cat_data.iter().find(|(_, id)| cat_list[0] == **id as u64) {
                        name.clone()
                    } else {
                        "".to_string()
                    }

                } else if let Some((name, _)) = cat_data.iter().find(|(_, id)| cat_list[0] == **id as u64) {
                        name.clone()
                } else { 
                    "".to_string()
                };
                name
            } else { 
                "".to_string() 
            };

            // println!("continent: {continent:?}");
                
            let tag_data = BTreeMap::<String, u32>::from_iter(
                tags.iter()
                    .map(|tag| (tag.name.clone(), tag.id.clone() as u32))
                    .filter(|(_, v)| tag_list.contains(&(*v as u64))),
            );

            let mut coord_set = BTreeSet::<Record<String, (f32, f32)>>::new();
            let coords = Vec::<Record<String, (f32, f32)>>::new();

            let mut scan_text = |text: &str| -> Result<(), Box<dyn Error>> {
                for result in re.find_iter(text.as_bytes()) {
                    let match_ = result?;
                    let match_str = match str::from_utf8(match_.as_bytes()) {
                        Ok(value) => value,
                        Err(_) => continue,
                    };
                    if let Some(city) = cities.iter().find(|c| {
                        match_str == c.admin_name || match_str == c.country || match_str == c.name
                    }) {
                        let coord = Record::new(match_str.to_string(), (city.lat, city.lng));
                        coord_set.insert(coord);
                    }
                }
                Ok(())
            };

            let category_text = cat_data
                .keys()
                .cloned()
                .collect::<Vec<String>>()
                .join(" ");
            scan_text(&category_text)?;
            scan_text(&title)?;
            scan_text(&excerpt)?;
            scan_text(&content_rendered)?;

            let mut captures_iter = img_re.captures_iter(content_rendered.as_bytes());
            // println!("captures_iter.count(): {}", &&captures_iter.count());
            while let Some(Ok(cap)) = captures_iter.next() {
                // println!("\ncaptures: {:?}", &captures);
                let url = match str::from_utf8(&cap["url"]) {
                    Ok(value) => value,
                    Err(_) => continue,
                };
                
                if let Some(a) = cap.name("alt") {
                    alt = String::from_utf8_lossy(a.as_bytes()).to_string();
                }
                if let Some(w) = cap.name("width") {
                    if let Ok(value) = str::from_utf8(w.as_bytes()) {
                        if let Ok(n) = u32::from_str_radix(value, 10) {
                        width = n;
                        }
                    }
                }
                if let Some(h) = cap.name("height") {
                    if let Ok(value) = str::from_utf8(h.as_bytes()) {
                        if let Ok(n) = u32::from_str_radix(value, 10) {
                        height = n;
                        }
                    }
                }

            }
            println!("fetching featured_media {featured_media}");

            let fg = Vec::<u8>::new();
            let bg = Vec::<u8>::new();

            if let Some((img_bytes, Some(mime_type))) = fetch_img(featured_media, &client, username.clone(), password.clone()).await? {
                
                info!("...converting to base64...");
                b64.push_str(format!("data:{};base64,", mime_type).as_str());

                b64.push_str(&img_bytes.to_base64(STANDARD));
                if b64 == "" {
                    println!("data for image with alt {alt:?} missing");
                }
                println!("done");

            }

            image = Some(Image {
                id: featured_media as u32,
                b64,
                width,
                height,
                alt,
                coords: JsonField::<Option<Record<String, (f32, f32)>>>(img_coords),
            });

            let row = entity::post::ActiveModel {
                id: Set(id as u32),
                date: Set(date),
                slug: Set(slug),
                link: Set(link),
                author: Set(author as u32),
                title: Set(title),
                content: Set(content_rendered),
                image: Set(JsonField::<Image>(image.expect("empty string"))),
                fg: Set("".to_string()),
                bg: Set("".to_string()),
                excerpt: Set(excerpt),
                continent: Set(continent),
                categories: Set(JsonField::<BTreeMap<String, u32>>(cat_data)),
                tags: Set(JsonField::<BTreeMap<String, u32>>(tag_data)),
                coords: Set(JsonField::<Option<Vec<Record<String, (f32, f32)>>>>(Some(
                    coords,
                ))),
            };

            // Tp5ds::insert(tp5d::ActiveModel {id:Set(featured_media as i32), fg: Set(format!("data:image/webp;base64,{}", fg.to_base64(STANDARD))), bg: Set(format!("data:image/webp;base64,{}", bg.to_base64(STANDARD))) }).exec(&db).await?;
            // let res = Posts::insert(row).exec(&db).await?;
            // println!("res: {res:?}");
            // // println!("{}", res.build(DbBackend::Sqlite).to_string());
            // res.exec(&db).await.ok();
        }

    }

    println!("done");
    server.await?;
    println!("finished");
    Ok(())
}

// async fn handle_socket(ws: WebSocketUpgrade, State(state): State<Arc<AppState>>) -> axum::response::Response {
//     ws.on_upgrade(move |socket| get_posts(socket, state))

// }

#[debug_handler]
async fn get_posts(
    State(state): State<Arc<AppState>>,
    Query(params): Query<PostsQuery>,
) -> Json<Vec<PostListItem>> {
    println!("GET /posts");

    // while let Some(msg) = ws.recv().await {
    //     if let Ok(msg) = msg {
    //         let id = msg.into_text()
    //     }
    // }
    let page = params.page.unwrap_or(1).max(1);
    let per_page = params
        .per_page
        .unwrap_or(DEFAULT_POSTS_PER_PAGE)
        .max(1)
        .min(MAX_POSTS_PER_PAGE);
    let offset = 7 + (page - 1) * per_page;

    let data = Posts::find()
        .select_only()
        .columns([
            post::Column::Id,
            post::Column::Date,
            post::Column::Title,
            post::Column::Slug,
            post::Column::Excerpt,
            post::Column::Image,
            post::Column::Fg,
            post::Column::Bg,
            post::Column::Link,
            post::Column::Continent,
            post::Column::Categories,
            post::Column::Tags,
            post::Column::Coords,
        ])
        .order_by(post::Column::Continent, sea_orm::Order::Asc)
        .order_by(post::Column::Date, sea_orm::Order::Desc)
        .offset(offset)
        .limit(per_page)
        .into_model::<PostListItem>()
        .all(&*state.db)
        .await
        .unwrap_or_default();
    Json(data)
}


#[debug_handler]
async fn get_post_ids(State(state): State<Arc<AppState>>) -> Json<Vec<u32>> {
    let ids = Posts::find()
        .select_only()
        .columns([
            post::Column::Id,
            post::Column::Date,
            post::Column::Title,
            post::Column::Image,
        ])
        .order_by_desc(post::Column::Date)
        .offset(7)
        .into_model::<PostIdRow>()
        .all(&*state.db)
        .await
        .unwrap_or_default();
    Json(
        ids.into_iter()
            .filter(|post| {
                if post.image.0.b64.as_str() != "" {
                    true
                } else {
                    println!("post with title {} has no feature image", post.title);
                    false
                }
            })
            .map(|post| post.id as u32)
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
async fn get_post_data(State(state): State<Arc<AppState>>, Path(id): Path<u64>) -> Json<Post> {
    let res = Posts::find_by_id(id as u32)
        // .select_only()
        .columns([
            post::Column::Id,
            post::Column::Date,
            post::Column::Title,
            post::Column::Slug,
            post::Column::Excerpt,
            post::Column::Image,
            post::Column::Link,
            post::Column::Continent,
            post::Column::Categories,
            post::Column::Tags,
            post::Column::Coords,
        ]);

    // println!("{}", res.build(DbBackend::Sqlite).to_string());
    let value: Json<post::Model>;
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
    let images = Posts::find()
        .select_only()
        .column(post::Column::Image)
        .order_by_desc(post::Column::Date)
        .limit(7)
        .into_model::<ImageRow>()
        .all(&*state.db)
        .await
        .unwrap_or_default();
    Json(images.into_iter().map(|row| row.image.0).collect())
}

#[debug_handler]
async fn get_featured_posts(State(state): State<Arc<AppState>>) -> Json<Vec<PostListItem>> {
    println!("get_featured_posts");
    let posts = Posts::find()
        .select_only()
        .columns([
            post::Column::Id,
            post::Column::Date,
            post::Column::Title,
            post::Column::Slug,
            post::Column::Excerpt,
            post::Column::Image,
            post::Column::Fg,
            post::Column::Bg,
            post::Column::Link,
            post::Column::Continent,
            post::Column::Categories,
            post::Column::Tags,
            post::Column::Coords,
        ])
        .order_by_desc(post::Column::Date)
        .limit(7)
        .into_model::<PostListItem>()
        .all(&*state.db)
        .await
        .unwrap_or_default();
    Json(posts)
}

#[debug_handler]
async fn get_image(State(state): State<Arc<AppState>>, Path(id): Path<u64>) -> Json<Image> {
    let direct = Posts::find_by_id(id as u32)
        .select_only()
        .column(post::Column::Image)
        .into_model::<ImageRow>()
        .one(&*state.db)
        .await
        .ok()
        .flatten();
    if let Some(image) = direct {
        return Json(image.image.0);
    }

    let res = Posts::find()
        .select_only()
        .column(post::Column::Image)
        .into_model::<ImageRow>()
        .all(&*state.db)
        .await
        .ok();
    if let Some(images) = res {
        if let Some(img) = images.into_iter().find(|i| i.image.0.id == id as u32) {
            Json(img.image.0)
        } else {
            Json(Image::default())
        }
    } else {
        Json(Image::default())
    }
}


#[debug_handler]
async fn get_menu_items(State(state): State<Arc<AppState>>) -> Json<Vec<MenuItem>> {
    let menu_items = MenuItems::find()
        .order_by_asc(menu_item::Column::MenuOrder)
        .all(&*state.db)
        .await
        .unwrap_or_default();
    Json(menu_items)
}


#[debug_handler]
async fn get_tp5d(State(state): State<Arc<AppState>>, Path(id): Path<u64>) -> Json<TP5D> {
    
    let res = Tp5ds::find_by_id(id as i32).one(&*state.db).await.unwrap_or_default().unwrap_or_default();
    Json(res)
}

async fn ping() -> String {
    "pong".to_string()
}

#[debug_handler]
async fn post_update_post(State(state): State<Arc<AppState>>, post: Json<Post>) {
    let id = post.id;
    Posts::update(post::ActiveModel::from(post.0));
    let mut lock = state.last_update.lock().unwrap();
    *lock = Utc::now();
}

#[debug_handler]
async fn last_update(State(state): State<Arc<AppState>>) -> Json<DateTime<Utc>> {
    let lock = state.last_update.lock().unwrap();
    Json(lock.clone())
}
