pub mod record {
    use serde::{Deserialize, Serialize};
    use std::{
        cmp::Ordering,
        fmt::{self, Display},
    };

    #[derive(Debug, Serialize, Deserialize, Hash, Clone)]
    pub struct Record<K, V: fmt::Debug>
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
}

pub mod city {

    use sea_orm::entity::prelude::*;
    use serde::Serialize;

    #[sea_orm::model]
    #[derive(Clone, Debug, DeriveEntityModel, PartialEq, Serialize)]
    #[sea_orm(table_name = "cities")]
    pub struct Model {
        #[sea_orm(primary_key, auto_increment = false)]
        pub id: u32,
        pub name: String,
        pub country: String,
        pub capital: String,
        pub admin_name: String,
        pub population: u32,
        pub lat: f32,
        pub lng: f32,
    }

    impl ActiveModelBehavior for ActiveModel {}
}

pub mod image {
    use super::record::Record;
    use sea_orm::JsonField;
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
    pub struct Image {
        pub id: u32,
        pub b64: String,
        pub width: u32,
        pub height: u32,
        pub alt: String,
        pub coords: JsonField<Option<Record<String, (f32, f32)>>>,
    }

    impl Default for Image {
        fn default() -> Self {
            Self {
                id: 0,
                b64: String::default(),
                width: 0,
                height: 0,
                alt: String::default(),
                coords: JsonField::<Option<Record<String, (f32, f32)>>>(None),
            }
        }
    }
}

pub mod post {
    use std::collections::BTreeMap;

    use super::image::Image;
    use super::record::Record;
    use sea_orm::{JsonField, entity::prelude::*};
    use serde::Serialize;

    // impl PartialEq for Image {
    //   fn cmp(&self, other: &Self) -> Ordering {
    //     if self.b64 < other.b64 { Ordering::Less } else { Ordering::Greater }
    //   }
    // }

    #[sea_orm::model]
    #[derive(Clone, Debug, DeriveEntityModel, Serialize)]
    #[sea_orm(table_name = "posts")]
    pub struct Model {
        #[sea_orm(primary_key)]
        pub id: u32,
        pub date: String,
        pub slug: String,
        pub link: String,
        pub author: u32,
        pub title: String,
        pub content: String,
        pub image: JsonField<Image>,
        pub fg: String,
        pub bg: String,
        pub excerpt: String,
        pub continent: String,
        pub categories: JsonField<BTreeMap<String, u32>>,
        pub tags: JsonField<BTreeMap<String, u32>>,
        pub coords: JsonField<Option<Vec<Record<String, (f32, f32)>>>>,
    }

    impl ActiveModelBehavior for ActiveModel {}

    impl Default for Model {
        fn default() -> Self {
            Self {
                id: 0,
                date: String::default(),
                slug: String::default(),
                link: String::default(),
                author: 0,
                title: String::default(),
                content: String::default(),
                image: JsonField::<Image>(Image::default()),
                fg: String::default(),
                bg: String::default(),
                excerpt: String::default(),
                continent: String::default(),
                categories: JsonField::<BTreeMap<String, u32>>(BTreeMap::<String, u32>::default()),
                tags: JsonField::<BTreeMap<String, u32>>(BTreeMap::<String, u32>::default()),
                coords: JsonField::<Option<Vec<Record<String, (f32, f32)>>>>(None),
            }
        }
    }
}


pub mod menu_item {
    use sea_orm::{JsonField, entity::prelude::*};
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Deserialize, Serialize, Clone, Default, PartialEq)]
    pub struct Html {
        pub rendered: String,
    }

    #[sea_orm::model]
    #[derive(Clone, Debug, DeriveEntityModel, PartialEq, Serialize, Deserialize)]
    #[sea_orm(table_name = "menu_items")]
    pub struct Model {
        #[sea_orm(primary_key)]
        pub id: u32,
        pub title: JsonField<Html>,
        pub parent: u32,
        pub menu_order: u32,
        pub url: String,
    }

    impl ActiveModelBehavior for ActiveModel {}
}

pub mod tp5d {
    use sea_orm::entity::prelude::*;
    use serde::{Deserialize, Serialize};

    #[sea_orm::model]
    #[derive(Clone, Debug, DeriveEntityModel, PartialEq, Serialize, Deserialize)]
    #[sea_orm(table_name = "tp5d")]
    pub struct Model {
        #[sea_orm(primary_key)]
        pub id: i32,
        pub fg: String,
        pub bg: String,
    }

    impl ActiveModelBehavior for ActiveModel {}

    impl Default for Model {
        fn default() -> Self {
            Model {
                id: -1i32,
                fg: "".to_string(),
                bg: "".to_string()
            }
        }
    }
}
