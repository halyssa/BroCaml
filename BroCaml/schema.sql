CREATE TABLE Users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT NOT NULL UNIQUE,
    password_hash TEXT NOT NULL,
    preferences TEXT
);

CREATE TABLE Eateries (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL UNIQUE,
    location TEXT
);

CREATE TABLE Reviews (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id INTEGER,
    eatery_id INTEGER,
    dish TEXT,
    rating INTEGER,
    comment TEXT,
    FOREIGN KEY (user_id) REFERENCES Users(id),
    FOREIGN KEY (eatery_id) REFERENCES Eateries(id)
);

CREATE TABLE Ratings (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    eatery_name TEXT NOT NULL,
    food_item TEXT NOT NULL,
    username TEXT NOT NULL,
    rating INTEGER CHECK(rating BETWEEN 1 AND 5),
    FOREIGN KEY (username) REFERENCES Users(username)
);

CREATE TABLE PersonalRatings (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    eatery_name TEXT NOT NULL,
    food_item TEXT NOT NULL,
    rating INTEGER CHECK(rating BETWEEN 1 AND 5)
);

