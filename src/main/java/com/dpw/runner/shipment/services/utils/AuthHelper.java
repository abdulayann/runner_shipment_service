package com.dpw.runner.shipment.services.utils;


import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;

public class AuthHelper {

    public static HttpHeaders getBridgeServiceHeaders(String xClient) {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.add("X-Client-Type", "STOS");
        return headers;
    }

    public static HttpHeaders getBridgeServiceTokenHeader(String xClient, String token) {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.add("X-Client-Type", "STOS");
        headers.add("Authorization", "Bearer "+ token);
        return headers;
    }
}
