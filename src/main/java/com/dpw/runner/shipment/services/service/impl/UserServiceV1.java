package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.service.interfaces.IUserService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.util.Arrays;
import java.util.Objects;

@Service
@Slf4j
@EnableCaching
public class UserServiceV1 implements IUserService {

    @Autowired
    private RestTemplate restTemplate;

    @Value("${v1-user-retrieve.url}")
    private String url;

    @Override
    public UsersDto getUserByToken(String token) {
        log.info("Request: {} || getUserByToken --- URL: {} ||| Token: {}", LoggerHelper.getRequestIdFromMDC(), url, token);
        HttpHeaders headers = new HttpHeaders();
        headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));
        if(token.split(" ").length <= 1 || !Objects.equals(token.split(" ")[0], "Bearer"))
            return null;
        token = token.split(" ")[1];
        headers.setBearerAuth(token);
        HttpEntity<String> entity = new HttpEntity<>(headers);
        ResponseEntity<UsersDto> responseEntity = restTemplate.exchange(url, HttpMethod.POST, entity, UsersDto.class);
        log.info("Request: {} || User retrieved from V1: {}", LoggerHelper.getRequestIdFromMDC(), responseEntity.getBody());
        return responseEntity.getBody();
    }

}
