package com.dpw.runner.shipment.services.validator;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.owasp.html.PolicyFactory;
import org.owasp.html.Sanitizers;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.util.ContentCachingRequestWrapper;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Iterator;
import java.util.Map;

@Component
public class XSSInterceptor implements HandlerInterceptor {

    private final PolicyFactory policy = Sanitizers.FORMATTING.and(Sanitizers.LINKS);

    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) throws Exception {
        ContentCachingRequestWrapper wrappedRequest = new ContentCachingRequestWrapper(request);
        String requestBody = getRequestBody(wrappedRequest);
        if (!requestBody.isEmpty()) {
            try {
                validateXSS(requestBody);
            } catch (Exception e) {
                response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
                response.getWriter().write(e.toString());
                response.getWriter().flush();
                response.getWriter().close();
                return false;
            }
        }
        return true;
    }

    private String getRequestBody(ContentCachingRequestWrapper request) throws IOException {
        StringBuilder requestBody = new StringBuilder();
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(request.getInputStream()))) {
            String line;
            while ((line = reader.readLine()) != null) {
                requestBody.append(line);
            }
        }
        return requestBody.toString();
    }

    public void validateXSS(String jsonString) throws Exception {
        ObjectMapper mapper = new ObjectMapper();
        JsonNode jsonNode = mapper.readTree(jsonString);
        Iterator<Map.Entry<String, JsonNode>> fieldsIterator = jsonNode.fields();
        while (fieldsIterator.hasNext()) {
            Map.Entry<String, JsonNode> fieldEntry = fieldsIterator.next();
            String fieldName = fieldEntry.getKey();
            JsonNode fieldValue = fieldEntry.getValue();
            if (fieldValue.isTextual() && containsXSS(fieldValue.asText())) {
                throw new RuntimeException("Use of Invalid characters. Please ensure no special characters used such as '<', '>' and '&'. Error occurred in field: " + fieldName);
            }
        }
    }

    private boolean containsXSS(String input) {
        String sanitizedInput = policy.sanitize(input);
        return !input.equals(sanitizedInput);
    }
}