package com.dpw.runner.shipment.services.customxmltojsonmapper;

import org.json.JSONArray;
import org.json.JSONObject;
import org.json.XML;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.parser.Parser;

public class XMLToJSONConverter {

    public static String context = "default";

    public static JSONObject convertXmlToJson(String xmlString) {
        JSONObject jsonObject = XML.toJSONObject(xmlString);
        return jsonObject;
    }

    public static JSONObject renameKeysOld(JSONObject jsonObject, Map<String, String> keyMapping) {
        JSONObject renamedObject = new JSONObject();
        Iterator<String> keys = jsonObject.keys();

        while (keys.hasNext()) {
            String key = keys.next();
            Object value = jsonObject.get(key);

            if (value instanceof JSONObject) {
                // Recursive call
                renamedObject.put(keyMapping.getOrDefault(key, key), renameKeysOld((JSONObject) value, keyMapping));
            } else {
                renamedObject.put(keyMapping.getOrDefault(key, key), value);
            }
        }
        return renamedObject;
    }

    private static JSONArray cleanJsonArray(JSONArray jsonArray) {
        JSONArray cleanedArray = new JSONArray();

        for (int i = 0; i < jsonArray.length(); i++) {
            Object value = jsonArray.get(i);
            if (value instanceof JSONObject) {
                cleanedArray.put(removeXmlnsEntries((JSONObject) value));
            } else if (value instanceof JSONArray) {
                cleanedArray.put(cleanJsonArray((JSONArray) value));
            } else {
                cleanedArray.put(value);
            }
        }

        return cleanedArray;
    }

    public static JSONObject removeXmlnsEntries(JSONObject jsonObject) {
        JSONObject cleanedJson = new JSONObject(jsonObject.toString());

        List<String> keysToRemove = new ArrayList<>();

        Iterator<String> keys = cleanedJson.keys();
        while (keys.hasNext()) {
            String key = keys.next();
            if (key.startsWith("xmlns:") || key.startsWith("xsi:")) {
                keysToRemove.add(key);
            } else {

                Object value = cleanedJson.get(key);
                if (value instanceof JSONObject) {
                    removeXmlnsEntries((JSONObject) value);
                }

                else if (value instanceof JSONArray) {
                    cleanJsonArray((JSONArray) value);
                }
            }
        }

        for (String key : keysToRemove) {
            cleanedJson.remove(key);
        }

        for (String key : cleanedJson.keySet()) {
            Object value = cleanedJson.get(key);
            if (value instanceof JSONObject) {
                cleanedJson.put(key, removeXmlnsEntries((JSONObject) value));
            } else if (value instanceof JSONArray) {
                cleanedJson.put(key, cleanJsonArray((JSONArray) value));
            }
        }
        return cleanedJson;
    }

    public static Map<String, String> loadKeyMappingsFromFile(String filePath) {
        try (FileReader reader = new FileReader(filePath)) {
            Gson gson = new Gson();
            return gson.fromJson(reader, new TypeToken<Map<String, String>>(){}.getType());
        } catch (Exception e) {
            e.printStackTrace();
            return new HashMap<>();
        }
    }


    private static String findNewKey(String key, Map<String, String> keyMapping) {
        for (Map.Entry<String, String> entry : keyMapping.entrySet()) {
            if (entry.getKey().startsWith(key) && entry.getKey().contains(context)) {
                return entry.getValue();
            }
        }

        return keyMapping.get(key);
    }

    private static void determineContext(JSONObject jsonObject) {
        if (jsonObject.keys().hasNext() && (jsonObject.keys().next().equals("Shipment") || jsonObject.keys().next().equals("Shipments")) && (context.equals("default") || context.equals("consol"))) {
            context = "shipment";
        } else if (jsonObject.keys().hasNext() && jsonObject.keys().next().equals("Consolidation") && (context.equals("default")  || context.equals("shipment"))) {
            context = "consol";
        }
    }

    public static JSONObject renameKeys(JSONObject jsonObject, Map<String, String> keyMapping) {
        JSONObject newJsonObject = new JSONObject();
        for (String key : jsonObject.keySet()) {
            Object value = jsonObject.get(key);
            String newKey = findNewKey(key, keyMapping);

            if (newKey != null) {
                if (value instanceof JSONObject) {
                    determineContext((JSONObject) value);
                    newJsonObject.put(newKey, renameKeys((JSONObject) value, keyMapping));
                } else if (value instanceof JSONArray) {

                    JSONArray newArray = new JSONArray();
                    for (int i = 0; i < ((JSONArray) value).length(); i++) {
                        newArray.put(renameKeys(((JSONArray) value).getJSONObject(i), keyMapping));
                    }
                    value = newArray;
                    newJsonObject.put(newKey, value);
                } else {

                    newJsonObject.put(newKey, value);
                }
            }
            else {
                if (value instanceof JSONObject) {
                    value = renameKeys((JSONObject) value, keyMapping);
                } else if (value instanceof JSONArray) {
                    JSONArray newArray = new JSONArray();
                    for (int i = 0; i < ((JSONArray) value).length(); i++) {
                        newArray.put(renameKeys(((JSONArray) value).getJSONObject(i), keyMapping));
                    }
                    value = newArray;
                }
                newJsonObject.put(key, value);
            }
        }
        return newJsonObject;
    }

    public static String removeNamespaces(String xmlString) {
        Document doc = Jsoup.parse(xmlString, "", Parser.xmlParser());

        doc.getAllElements().forEach(element -> {
            element.tagName(element.tagName().replaceAll(".*:", ""));
        });

        return doc.outerHtml();
    }


    public static void main(String[] args){

        // Path to the XML file
        Path filePath = Paths.get("/Users/Aditya.Thakur/Documents/runner_shipment_service/src/main/java/com/dpw/runner/shipment/services/customxmltojsonmapper/shipment.xml");

        try {
            String xmlString = Files.readString(filePath);

            String xmlWithoutNamespaces = removeNamespaces(xmlString);
            JSONObject jsonObject = convertXmlToJson(xmlWithoutNamespaces);
            System.out.println("Original JSON: " + jsonObject.toString(4));

            JSONObject cleanedJson = removeXmlnsEntries(jsonObject);
            System.out.println("Cleaned JSON: " + cleanedJson.toString(4));

            Map<String, String> keyMappings = loadKeyMappingsFromFile("/Users/Aditya.Thakur/Documents/runner_shipment_service/src/main/java/com/dpw/runner/shipment/services/customxmltojsonmapper/keyMappings.json");


            determineContext(jsonObject);
            JSONObject renamedJson = renameKeys(cleanedJson, keyMappings);


            //  JSONObject renamedJson = renameKeys(cleanedJson, keyMappings);

            System.out.println("Modified JSON: " + renamedJson.toString(4));
        } catch (IOException ex) {
            ex.printStackTrace();
        }

    }
}

