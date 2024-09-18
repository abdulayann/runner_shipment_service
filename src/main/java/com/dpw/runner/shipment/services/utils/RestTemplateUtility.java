package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.EAWBConstants;
import org.apache.commons.codec.binary.Base64;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.util.ObjectUtils;

import java.nio.charset.StandardCharsets;

public class RestTemplateUtility {

    public static MultiValueMap<String, String> getCustomDescartesHeaders(String username, String password) {
        MultiValueMap<String, String> headers = new LinkedMultiValueMap<>();
        headers.add(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE);
        headers.add(HttpHeaders.AUTHORIZATION, createBasicAuthToken(username, password));
        headers.add(HttpHeaders.ACCEPT, EAWBConstants.DESCARTES_ACCEPT_HEADER);

        return headers;
    }

    private static String createBasicAuthToken(String username, String password) {
        if (ObjectUtils.isEmpty(username) || ObjectUtils.isEmpty(password)) {
            return null;
        }
        String auth = username + ":" + password;
        byte[] encodedAuth = Base64.encodeBase64(auth.getBytes(StandardCharsets.US_ASCII));
        return "Basic " + new String(encodedAuth);
    }
}
