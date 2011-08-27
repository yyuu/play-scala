--- Downs can be configured before Ups

# --- !Downs

DROP TABLE Tag;

# --- !Ups

CREATE TABLE Tag (
    id bigint(20) NOT NULL AUTO_INCREMENT,
    name varchar(255) NOT NULL,
    PRIMARY KEY (id)
);

