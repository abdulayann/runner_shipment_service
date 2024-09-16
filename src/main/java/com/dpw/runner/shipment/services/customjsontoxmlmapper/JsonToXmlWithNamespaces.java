package com.dpw.runner.shipment.services.customjsontoxmlmapper;

import org.json.JSONObject;
import org.json.XML;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class JsonToXmlWithNamespaces {

    public static void main(String[] args) {

        String filePath = "/Users/Aditya.Thakur/Documents/runner_shipment_service/src/main/java/com/dpw/runner/shipment/services/customjsontoxmlmapper/data.json";

        try {
            // Read the JSON file as a string
            String jsonString = new String(Files.readAllBytes(Paths.get(filePath)));

            // Print the JSON string
            System.out.println(jsonString);

            // Convert JSON to XML with namespaces
            String xmlWithNamespaces = jsonToXmlWithNamespaces(jsonString);

            // Output the result
            System.out.println(xmlWithNamespaces);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    // Convert JSON to XML and add namespaces to the root element
    public static String jsonToXmlWithNamespaces(String jsonString) {
        // Parse the JSON object
        JSONObject json = new JSONObject(jsonString);

        // Convert JSON to XML
        String rawXml = XML.toString(json);

        // Add the namespaces to the root element
        String xmlWithNamespaces = """
            <Waybill xmlns:ccts="urn:un:unece:uncefact:documentation:standard:CoreComponentsTechnicalSpecification:2"
                    xmlns:udt="urn:un:unece:uncefact:data:standard:UnqualifiedDataType:8"
                    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                    xmlns:rsm="iata:waybill:1"
                    xmlns:ram="iata:datamodel:3"
                    xsi:schemaLocation="iata:waybill:1 Waybill_1.xsd">
                """ + rawXml + "\n</Waybill>";

        return xmlWithNamespaces;
    }
}
