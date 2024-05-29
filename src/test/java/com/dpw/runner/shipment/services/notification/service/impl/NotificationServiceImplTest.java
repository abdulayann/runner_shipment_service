package com.dpw.runner.shipment.services.notification.service.impl;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.document.util.BASE64DecodedMultipartFile;
import com.dpw.runner.shipment.services.notification.config.NotificationConfig;
import com.dpw.runner.shipment.services.notification.config.NotificationRestClient;
import com.dpw.runner.shipment.services.notification.request.NotificationServiceSendEmailRequest;
import com.dpw.runner.shipment.services.notification.request.SendEmailBaseRequest;
import com.dpw.runner.shipment.services.notification.response.NotificationServiceResponse;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.UnsupportedEncodingException;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.PropertySource;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ContextConfiguration(classes = {NotificationServiceImpl.class, NotificationConfig.class})
@ExtendWith(SpringExtension.class)
@PropertySource("classpath:application-test.properties")
@EnableConfigurationProperties
class NotificationServiceImplTest {
    @MockBean
    private NotificationRestClient notificationRestClient;

    @Autowired
    private NotificationServiceImpl notificationServiceImpl;

    @MockBean
    private ObjectMapper objectMapper;

    @Test
    void testSendEmail() throws JsonProcessingException, UnsupportedEncodingException {
        // Arrange
        when(objectMapper.writeValueAsString(Mockito.<Object>any())).thenReturn("42");

        NotificationServiceResponse notificationServiceResponse = new NotificationServiceResponse();
        notificationServiceResponse.setAcknowledgementId("42");
        notificationServiceResponse.setErrorCode("An error occurred");
        notificationServiceResponse.setMessage("Not all who wander are lost");
        notificationServiceResponse.setSuccess("Success");
        when(notificationRestClient.sendEmail(Mockito.<NotificationServiceSendEmailRequest>any()))
                .thenReturn(notificationServiceResponse);

        SendEmailBaseRequest request = new SendEmailBaseRequest();
        request.setBcc("ada.lovelace@example.org");
        request.setBranchId("janedoe/featurebranch");
        request.setCc("ada.lovelace@example.org");
        request.setFile(new BASE64DecodedMultipartFile("AXAXAXAX".getBytes("UTF-8")));
        request.setHtmlBody("Not all who wander are lost");
        request.setItem("Item");
        request.setModuleName("Module Name");
        request.setSubject("Hello from the Dreaming Spires");
        request.setTemplateName("Template Name");
        request.setTo("alice.liddell@example.org");
        request.setUserId("42");
        request.setUserName("janedoe");

        // Act
        NotificationServiceResponse actualSendEmailResult = notificationServiceImpl.sendEmail(request);

        // Assert
        verify(notificationRestClient).sendEmail(isA(NotificationServiceSendEmailRequest.class));
        verify(objectMapper, atLeast(1)).writeValueAsString(Mockito.<Object>any());
        assertSame(notificationServiceResponse, actualSendEmailResult);
    }

    @Test
    void testSendEmail2() throws JsonProcessingException, UnsupportedEncodingException {
        // Arrange
        when(objectMapper.writeValueAsString(Mockito.<Object>any())).thenReturn("42");
        when(notificationRestClient.sendEmail(Mockito.<NotificationServiceSendEmailRequest>any()))
                .thenThrow(new RuntimeException("high"));

        SendEmailBaseRequest request = new SendEmailBaseRequest();
        request.setBcc("ada.lovelace@example.org");
        request.setBranchId("janedoe/featurebranch");
        request.setCc("ada.lovelace@example.org");
        request.setFile(new BASE64DecodedMultipartFile("AXAXAXAX".getBytes("UTF-8")));
        request.setHtmlBody("Not all who wander are lost");
        request.setItem("Item");
        request.setModuleName("Module Name");
        request.setSubject("Hello from the Dreaming Spires");
        request.setTemplateName("Template Name");
        request.setTo("alice.liddell@example.org");
        request.setUserId("42");
        request.setUserName("janedoe");

        // Act and Assert
        assertThrows(RuntimeException.class, () -> notificationServiceImpl.sendEmail(request));
        verify(notificationRestClient).sendEmail(isA(NotificationServiceSendEmailRequest.class));
        verify(objectMapper, atLeast(1)).writeValueAsString(Mockito.<Object>any());
    }

    @Test
    void testSendEmail3() throws JsonProcessingException, UnsupportedEncodingException {
        // Arrange
        when(objectMapper.writeValueAsString(Mockito.<Object>any())).thenThrow(new JsonProcessingException("a"){});
        NotificationServiceResponse notificationServiceResponse = new NotificationServiceResponse();
        notificationServiceResponse.setAcknowledgementId("42");
        notificationServiceResponse.setErrorCode("An error occurred");
        notificationServiceResponse.setMessage("Not all who wander are lost");
        notificationServiceResponse.setSuccess("Success");
        when(notificationRestClient.sendEmail(Mockito.<NotificationServiceSendEmailRequest>any()))
                .thenReturn(notificationServiceResponse);
        SendEmailBaseRequest request = new SendEmailBaseRequest();
        request.setBcc("ada.lovelace@example.org");
        request.setBranchId("janedoe/featurebranch");
        request.setCc("ada.lovelace@example.org");
        request.setFile(new BASE64DecodedMultipartFile("AXAXAXAX".getBytes("UTF-8")));
        request.setHtmlBody("Not all who wander are lost");
        request.setItem("Item");
        request.setModuleName("Module Name");
        request.setSubject("Hello from the Dreaming Spires");
        request.setTemplateName("Template Name");
        request.setTo("alice.liddell@example.org");
        request.setUserId("42");
        request.setUserName("janedoe");

        // Act and Assert
        assertThrows(RuntimeException.class, () -> notificationServiceImpl.sendEmail(request));
        verify(objectMapper, atLeast(1)).writeValueAsString(Mockito.<Object>any());
    }
}
