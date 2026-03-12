# places-ai

A Rust/Axum microservice backend for [ozimage](https://ozimage.com.au) — a photography and travel website. The service fetches, reorganises, and serves content from a WordPress site to a Next.js/React frontend.

> **Status: Work in progress, built for a specific deployment.** This is a purpose-built microservice for a particular site and is not a general-purpose library.

## Overview

`places-ai` sits between a WordPress content source and a Next.js frontend, providing:

- Structured REST endpoints for posts, cities, menu items, and image content
- GPS coordinate extraction and association for cities mentioned in articles
- Image metadata handling (EXIF, base64 encoding) and CSV-based data ingestion
- SQLite persistence via SeaORM for improved query performance over direct WordPress API calls

The "AI" in the name reflects an early ambition to use k-means clustering for content organisation — that approach was abandoned, and no ML/AI currently runs in this service.

## Data model

| Entity | Description |
|--------|-------------|
| `Posts` | WordPress posts fetched and cached locally |
| `Cities` | Locations referenced in content, with GPS coordinates |
| `MenuItems` | Navigation/menu structure |
| `Images` | Image records with EXIF metadata |
| `Records` | General-purpose log/audit records |

## Running

Set up a `.env` file with the SQLite database path and WordPress API base URL, then:

```bash
cargo run
```

## Built with

- [`axum`](https://github.com/tokio-rs/axum) — HTTP framework
- [`sea-orm`](https://github.com/SeaQL/sea-orm) — async ORM, SQLite backend
- [`pcre2`](https://github.com/nicowillis/rust-pcre2) — regex for content parsing
- [`little_exif`](https://github.com/TechnikTobi/little_exif) — EXIF metadata extraction
- [`b64-ct`](https://crates.io/crates/b64-ct) — constant-time base64 encoding
- [`csv`](https://github.com/BurntSushi/rust-csv) — CSV data ingestion
