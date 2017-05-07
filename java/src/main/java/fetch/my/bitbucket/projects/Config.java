package fetch.my.bitbucket.projects;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.nio.file.Path;

import static java.util.Objects.requireNonNull;

final class Config {

    private final String username;
    private final String password;
    private final Path directory;

    @JsonCreator
    Config(
            @JsonProperty(value = "username", required = true) String username,
            @JsonProperty(value = "password", required = true) String password,
            @JsonProperty(value = "directory", required = true) Path directory
    ) {
        this.username = requireNonNull(username, "username");
        this.password = requireNonNull(password, "password");
        this.directory = requireNonNull(directory, "directory");
    }

    String username() {
        return username;
    }

    String password() {
        return password;
    }

    Path directory() {
        return directory;
    }

}
