# Initial data

# --- !Ups

INSERT INTO contact VALUES (NULL, 'Guillaume', 'Bort', '1980-12-21', 'gbo@zenexity.com');

# --- !Downs

DELETE FROM contact WHERE email = 'gbo@zenexity.com';