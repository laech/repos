package fetch.my.bitbucket.projects;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.io.IOUtils;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Comparator;
import java.util.List;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.Collections.unmodifiableList;
import static java.util.stream.Collectors.toList;
import static java.util.stream.StreamSupport.stream;

class Main {

    public static void main(String[] args) throws Exception {

        if (args.length != 1) {
            System.err.println("Usage: java -jar <jar> <config.json>");
            System.exit(1);
        }

        ObjectMapper mapper = new ObjectMapper();

        Config config = mapper.readValue(new File(args[0]), Config.class);

        System.out.println("Fetching projects...");
        List<String> sshUrls = getSshUrls(config.username(), config.password(), mapper);
        if (sshUrls.isEmpty()) {
            System.out.println("No project found.");
        } else {
            System.out.println("Found the following projects:");
            sshUrls.forEach(System.out::println);
        }

        for (String sshUrl : sshUrls) {
            System.out.println("Fetching " + sshUrl);
            fetchProject(config.directory(), sshUrl);
        }
    }

    private static void fetchProject(Path parentDirectory, String sshUrl)
            throws IOException, InterruptedException {

        String name = getProjectNameFromSshUrl(sshUrl);
        Path directory = parentDirectory.resolve(name);
        ProcessBuilder processBuilder;
        if (Files.exists(directory)) {
            processBuilder = new ProcessBuilder("git", "fetch")
                    .directory(directory.toFile());
        } else {
            processBuilder = new ProcessBuilder("git", "clone", sshUrl)
                    .directory(parentDirectory.toFile());
        }
        int exitCode = processBuilder.inheritIO().start().waitFor();
        if (exitCode != 0) {
            throw new IllegalStateException(
                    "Process returned exit code: " + exitCode);
        }
    }

    private static String getProjectNameFromSshUrl(String sshUrl) {
        int i = sshUrl.lastIndexOf('/');
        if (i < 0) {
            throw new IllegalStateException("Unknown project URL: " + sshUrl);
        }
        return sshUrl.substring(i + 1).replaceAll("\\.git$", "");
    }

    private static List<String> getSshUrls(
            String username,
            String password,
            ObjectMapper mapper
    ) throws IOException {

        List<String> sshUrls = new ArrayList<>();
        URL url = new URL("https://bitbucket.org/api/2.0/repositories/" + username);
        while (url != null) {
            try (InputStream in = connect(url, username, password)) {
                String json = IOUtils.toString(in, UTF_8);
                try {
                    JsonNode response = mapper.readTree(json);
                    sshUrls.addAll(parseSshUrls(response));
                    url = getNextPageUrl(response);
                } catch (Throwable e) {
                    throw new IllegalStateException("Failed to process JSON: " + json, e);
                }
            }
        }
        sshUrls.sort(Comparator.naturalOrder());
        return unmodifiableList(sshUrls);
    }

    private static URL getNextPageUrl(JsonNode response) throws MalformedURLException {
        JsonNode next = response.get("next");
        return next != null ? new URL(next.textValue()) : null;
    }

    private static InputStream connect(URL url, String username, String password) throws IOException {
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();
        String auth = Base64.getEncoder().encodeToString((username + ":" + password).getBytes(UTF_8));
        connection.setRequestProperty("Authorization", "Basic " + auth);
        if (connection.getResponseCode() != 200) {
            closeAndThrowIfNotNull(connection.getErrorStream());
            closeAndThrowIfNotNull(connection.getInputStream());
        }
        return connection.getInputStream();
    }

    private static List<String> parseSshUrls(JsonNode response) {
        return stream(response.get("values").spliterator(), false)
                .flatMap(project -> stream(project.get("links").get("clone").spliterator(), false))
                .filter(link -> link.get("name").textValue().equals("ssh"))
                .map(link -> link.get("href").textValue())
                .collect(toList());
    }

    private static void closeAndThrowIfNotNull(InputStream in) throws IOException {
        try (InputStream copy = in) {
            if (in != null) {
                throw new IllegalStateException(IOUtils.toString(copy, UTF_8));
            }
        }
    }
}
