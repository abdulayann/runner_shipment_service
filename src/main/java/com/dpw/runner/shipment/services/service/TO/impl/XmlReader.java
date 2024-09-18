package com.dpw.runner.shipment.services.service.TO.impl;

import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.io.InputStream;
import java.util.Scanner;

@Service
public class XmlReader {

    private final ResourceLoader resourceLoader;

    public XmlReader(ResourceLoader resourceLoader) {
        this.resourceLoader = resourceLoader;
    }

    public String readXmlTemplate(String templateName) throws IOException {
        // Load the XML file from the classpath
        Resource resource = resourceLoader.getResource("classpath:xmls/" + templateName);

        // Read the content of the XML file
        try (InputStream inputStream = resource.getInputStream();
             Scanner scanner = new Scanner(inputStream, "UTF-8").useDelimiter("\\A")) {
            return scanner.hasNext() ? scanner.next() : "";
        }
    }
}
