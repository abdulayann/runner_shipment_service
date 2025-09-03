package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;

import org.springframework.stereotype.Component;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Component
public class VersionContext {
    private VersionContext(){}
    private static ThreadLocal<String> version = new InheritableThreadLocal<>();

    private static final Pattern VERSION_PATTERN = Pattern.compile("/api/(v\\d+)/", Pattern.CASE_INSENSITIVE);

    public static void setVersionFromPath(String path) {
        Matcher matcher = VERSION_PATTERN.matcher(path);
        if (matcher.find()) {
            String extractedVersion = matcher.group(1).toUpperCase();
            version.set(extractedVersion);
        } else {
            version.remove();
        }
    }

    public static String getVersion() {
        return version.get();
    }

    public static void remove() {
        version.remove();
    }

}
