use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

pub fn init_logging() {
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "lantae=info".into()),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();
}

pub fn init_logging_with_level(level: &str) {
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| format!("lantae={}", level).into()),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();
}