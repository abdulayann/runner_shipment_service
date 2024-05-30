package com.dpw.runner.shipment.services.helper;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.*;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

@Component
public class TokenFetcher {

    private final RestTemplate restTemplate;

    @Autowired
    public TokenFetcher(RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }

    public String fetchUserToken() {
        String url = "https://qa-runner.cargoes.com/Api/Account/GenerateToken";
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        // Add any required cookies to the request headers
//        headers.add("Cookie", ".AspNetCore.Cookies=CfDJ8OimhIn3AhxGivVLfc-jxCOKK2MscWb3izLqCi8Vi1FN9XF_syx4FwEowtURpN7KJB4lLHTVyiKRw1H-4z1DcchdfCFAtY-vEJje13vy2ScHmkDWbdusLA4-TTs-3MZ97no5HXlmOjlbQDxRNoyxYFqCgDa-cor0BOXBdrILIQnpvpR2wg7_pWmLY2MhpJ2J_rNkmrrb7n4dGgdHUkhTXVMuTHejTROhL_kBPK1F6VYbNEfrtgfWPZjDVOezifDnUBnS6TmjbjPhkAynlraXKVnIYjcXe-UQvFIyctrzxNwO; AKSCookie=1709900077.502.3295.82383|2de5613fa8d3b587a93f0be80793c7db; cookiesession1=678A40D20E03046ACF9C51B5E7350EA7");

        // Request body
        String requestBody = "{\"Username\": \"hipl2\", \"Password\": \"Runner@123\"}";

        HttpEntity<String> requestEntity = new HttpEntity<>(requestBody, headers);

        ResponseEntity<String> responseEntity = restTemplate.exchange(url, HttpMethod.POST, requestEntity, String.class);

        if (responseEntity.getStatusCode() == HttpStatus.OK) {
            // Parse response and extract token
            String responseBody = responseEntity.getBody();
            // Assuming the token is present in a specific format in the response body
            // Extract token from response body and return it
            // Example:
            // String token = parseTokenFromResponseBody(responseBody);
            // return token;
            return responseBody; // Temporary return response body for demonstration
        } else {
            throw new RuntimeException("Failed to fetch user token. Status code: " + responseEntity.getStatusCodeValue());
        }
    }
}
