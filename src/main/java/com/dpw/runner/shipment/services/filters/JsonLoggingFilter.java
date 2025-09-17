package com.dpw.runner.shipment.services.filters;

import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;
import org.springframework.web.util.ContentCachingRequestWrapper;
import org.springframework.web.util.ContentCachingResponseWrapper;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

@Component
@Slf4j
public class JsonLoggingFilter extends OncePerRequestFilter {
    @Override
    protected void doFilterInternal(HttpServletRequest request,
                                    HttpServletResponse response,
                                    FilterChain filterChain) throws ServletException, IOException {
        long startRequest = System.currentTimeMillis();
        ContentCachingRequestWrapper requestWrapper = new ContentCachingRequestWrapper(request);
        ContentCachingResponseWrapper responseWrapper = new ContentCachingResponseWrapper(response);

        String method = request.getMethod();
        String url = request.getRequestURI();
        String query = request.getQueryString();

        Map<String, String> headers = new HashMap<>();
        Enumeration<String> headerNames = request.getHeaderNames();
        while (headerNames.hasMoreElements()) {
            String name = headerNames.nextElement();
            String value = name.equalsIgnoreCase("authorization") ? "*****" : request.getHeader(name);
            headers.put(name, value);
        }

        long start = System.currentTimeMillis();

        String requestBody = "";
        if (isJsonContent(request)) {
            requestBody = new String(requestWrapper.getContentAsByteArray(), requestWrapper.getCharacterEncoding());
        }
        log.info("{} | REQUEST RECEIVED [HTTP Method={}] [URL={}] [LOGGING_DURATION = {} ms] [QUERY={}] [HEADERS={}] [BODY={}]", LoggerHelper.getRequestIdFromMDC(),
                method,
                url,
                System.currentTimeMillis() - startRequest,
                query != null ? query : "",
                headers,
                requestBody);

        try {
            filterChain.doFilter(requestWrapper, responseWrapper);
        } finally {
            long endRequest = System.currentTimeMillis();
            long duration = System.currentTimeMillis() - start;

            String responseBody = "";
            if (isJsonResponse(responseWrapper)) {
                responseBody = new String(responseWrapper.getContentAsByteArray(), responseWrapper.getCharacterEncoding());
            }
            log.info("{} | RESPONSE RETURNED [URL={}] [LOGGING_DURATION = {} ms] [DURATION = {} ms] [RESPONSE={}]",
                    LoggerHelper.getRequestIdFromMDC(), url, System.currentTimeMillis() - endRequest, duration, responseBody);

            responseWrapper.copyBodyToResponse();
        }
    }

    private boolean isJsonContent(HttpServletRequest request) {
        String contentType = request.getContentType();
        return contentType != null && contentType.toLowerCase().contains("application/json");
    }

    private boolean isJsonResponse(HttpServletResponse response) {
        String contentType = response.getContentType();
        return contentType != null && contentType.toLowerCase().contains("application/json");
    }
}
