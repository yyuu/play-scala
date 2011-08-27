--- Downs can be configured before Ups

# --- !Downs

DROP TABLE TagsForPosts;

# --- !Ups

CREATE TABLE TagsForPosts (
    id bigint(20) NOT NULL AUTO_INCREMENT,
    tag_id bigint(20) DEFAULT NULL,
    post_id bigint(20) DEFAULT NULL,
    PRIMARY KEY (id),
    FOREIGN KEY (post_id) REFERENCES Post(id) ON DELETE CASCADE,
    FOREIGN KEY (tag_id) REFERENCES Tag(id) ON DELETE CASCADE
);
