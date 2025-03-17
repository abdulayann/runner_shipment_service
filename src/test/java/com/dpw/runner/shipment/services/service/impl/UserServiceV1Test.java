package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.dto.request.UsersDto;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.*;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class UserServiceV1Test {

    @Mock
    private RestTemplate restTemplate;

    @InjectMocks
    private UserServiceV1 userServiceV1;

    private String url = "http://example.com";
    private String validToken = "Bearer validToken";
    private String invalidToken = "invalidToken";

    @BeforeEach
    void setUp() {
        ReflectionTestUtils.setField(userServiceV1, "url", url);
    }

    @Test
    void getUserByToken_ValidToken_ReturnsUserDto() {
        HttpHeaders headers = new HttpHeaders();
        headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));
        headers.setBearerAuth("validToken");
        HttpEntity<String> entity = new HttpEntity<>(headers);
        UsersDto expectedUserDto = new UsersDto();
        ResponseEntity<UsersDto> responseEntity = new ResponseEntity<>(expectedUserDto, HttpStatus.OK);
        when(restTemplate.exchange(eq(url), eq(HttpMethod.POST), eq(entity), eq(UsersDto.class))).thenReturn(responseEntity);

        UsersDto actualUserDto = userServiceV1.getUserByToken(validToken);

        assertEquals(expectedUserDto, actualUserDto);
        verify(restTemplate, times(1)).exchange(eq(url), eq(HttpMethod.POST), eq(entity), eq(UsersDto.class));
    }

    @Test
    void getUserByToken_InvalidToken_ReturnsNull() {
        HttpHeaders headers = new HttpHeaders();
        headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));
        HttpEntity<String> entity = new HttpEntity<>(headers);

        UsersDto actualUserDto = userServiceV1.getUserByToken(invalidToken);

        assertEquals(null, actualUserDto);
        verify(restTemplate, never()).exchange(anyString(), any(HttpMethod.class), any(HttpEntity.class), eq(UsersDto.class));
    }
}
