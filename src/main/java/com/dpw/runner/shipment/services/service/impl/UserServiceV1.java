package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.service.interfaces.IUserService;
import com.dpw.runner.shipment.services.utils.TokenUtility;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

@Service
@Slf4j
public class UserServiceV1 implements IUserService {

    static Map<String, UsersDto> userDefinition = new HashMap<>();
    @Autowired
    private RestTemplate restTemplate;

    @Value("${v1-user-retrieve.url}")
    private String url;

    @Autowired
    private TokenUtility tokenUtility;

    @Override
    public UsersDto getUserByToken(String token) {
        log.info("getUserByToken --- URL: {} ||| Token: {}", url, token);
        HttpHeaders headers = new HttpHeaders();
        headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));
        if(token.split(" ").length <= 1 || !Objects.equals(token.split(" ")[0], "Bearer"))
            return null;
        token = token.split(" ")[1];

        /** Check if user details exits in memory cache **/
        String key = tokenUtility.getUserIdAndBranchId(token);
        if (!Objects.isNull(key) && userDefinition.containsKey(key) && userDefinition.get(key).getValidity() > System.currentTimeMillis()) {
            log.info("User Details Retrieved from memory Cache");
            return userDefinition.get(key);
        }

        headers.setBearerAuth(token);
        HttpEntity<String> entity = new HttpEntity<String>(headers);
        ResponseEntity<UsersDto> responseEntity = restTemplate.exchange(url, HttpMethod.POST, entity, UsersDto.class);
        log.info("User retrieved: " + responseEntity.getBody());

        /** Create/Update data in memory cache **/
        if (!Objects.isNull(key)) {
            log.info("User Details Set in memory Cache for user key: {}", key);
            responseEntity.getBody().setValidity(System.currentTimeMillis() + (15 * 60 * 1000));
            userDefinition.put(key, responseEntity.getBody());
        }
//        TODO - Scheduled invalidating the memory
        return responseEntity.getBody();
    }



}
