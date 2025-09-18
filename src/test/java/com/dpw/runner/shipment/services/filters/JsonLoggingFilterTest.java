package com.dpw.runner.shipment.services.filters;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;

@ExtendWith({SpringExtension.class, MockitoExtension.class})
class JsonLoggingFilterTest {

    @Mock
    private FilterChain filterChain;

    @InjectMocks
    private JsonLoggingFilter jsonLoggingFilter;


    @Test
    void testDoFilterInternal_logsRequestAndResponse() throws ServletException, IOException {
        MockHttpServletRequest mockRequest = new MockHttpServletRequest();
        mockRequest.setMethod("POST");
        mockRequest.setRequestURI("/test");
        mockRequest.setContentType("application/json");
        mockRequest.setContent("{\"key\":\"value\"}".getBytes());
        mockRequest.addHeader("Authorization", "Bearer token123");

        MockHttpServletResponse mockResponse = new MockHttpServletResponse();
        mockResponse.setContentType("application/json");
        mockResponse.getOutputStream().write("{\"result\":\"ok\"}".getBytes());

        doAnswer(invocation -> {
            return null;
        }).when(filterChain).doFilter(any(), any());

        jsonLoggingFilter.doFilterInternal(mockRequest, mockResponse, filterChain);

        assertEquals("{\"result\":\"ok\"}", mockResponse.getContentAsString());
    }

}