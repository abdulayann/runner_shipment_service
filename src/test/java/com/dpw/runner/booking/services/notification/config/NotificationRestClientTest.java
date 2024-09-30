package com.dpw.runner.booking.services.notification.config;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.booking.services.adapters.impl.ReportServiceAdapter;
import com.dpw.runner.booking.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.booking.services.dto.request.UsersDto;
import com.dpw.runner.booking.services.notification.request.NotificationServiceSendEmailRequest;
import com.dpw.runner.booking.services.notification.response.NotificationServiceResponse;

import java.io.UnsupportedEncodingException;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

@ContextConfiguration(classes = {NotificationRestClient.class, NotificationConfig.class})
@ExtendWith(SpringExtension.class)
@PropertySource("classpath:application-test.properties")
@EnableConfigurationProperties
class NotificationRestClientTest {
    @Autowired
    private NotificationRestClient notificationRestClient;

    @MockBean
    private ReportServiceAdapter reportServiceAdapter;

    @MockBean
    private RestTemplate restTemplate;

    @Test
    void testSendEmail() throws UnsupportedEncodingException, RestClientException {
        // Arrange
        UserContext.setUser(UsersDto.builder().Id(1).Username("a").TenantId(1).build());
        when(restTemplate.exchange(Mockito.<String>any(), Mockito.<HttpMethod>any(), Mockito.<HttpEntity<Object>>any(),
                Mockito.<Class<Object>>any(), (Object[]) any())).thenReturn(new ResponseEntity<>(HttpStatus.CONTINUE));

        NotificationServiceSendEmailRequest params = new NotificationServiceSendEmailRequest();
        params.setApplicationId("42");
        params.setBccEmails("jane.doe@example.org");
        params.setCcEmails("jane.doe@example.org");
        params.setFiles(new MockMultipartFile("Ancd", "Abcd".getBytes("UTF-8")));
        params.setHtmlBody("Not all who wander are lost");
        params.setItem("Item");
        params.setMetadata("Metadata");
        params.setModuleName("Module Name");
        params.setOrganizationId("42");
        params.setPriority("Priority");
        params.setSubject("Hello from the Dreaming Spires");
        params.setTemplateName("Template Name");
        params.setTrackEmailEvents(true);

        // Act
        NotificationServiceResponse actualSendEmailResult = notificationRestClient.sendEmail(params);

        // Assert
        verify(restTemplate).exchange(
                eq("http://staging-notification-service-api.private-cargoes.com/emailTemplates/sendEmail"), eq(HttpMethod.POST),
                isA(HttpEntity.class), isA(Class.class), (Object[]) any());
        assertNull(actualSendEmailResult);
    }

    @Test
    void testSendEmail2() throws UnsupportedEncodingException, RestClientException {
        // Arrange
        when(restTemplate.exchange(Mockito.<String>any(), Mockito.<HttpMethod>any(), Mockito.<HttpEntity<Object>>any(),
                Mockito.<Class<Object>>any(), (Object[]) any())).thenReturn(new ResponseEntity<>(HttpStatus.CONTINUE));

        NotificationServiceSendEmailRequest params = new NotificationServiceSendEmailRequest();
        params.setApplicationId("42");
        params.setBccEmails("jane.doe@example.org");
        params.setCcEmails("jane.doe@example.org");
        params.setFiles(new MockMultipartFile("Ancd", "Abcd".getBytes("UTF-8")));
        params.setHtmlBody("Not all who wander are lost");
        params.setItem("Item");
        params.setMetadata("Metadata");
        params.setModuleName("Module Name");
        params.setOrganizationId("42");
        params.setPriority("Priority");
        params.setRecipientEmails("jane.doe@example.org");
        params.setSubject("Hello from the Dreaming Spires");
        params.setTemplateName("Template Name");
        params.setTrackEmailEvents(true);

        // Act
        NotificationServiceResponse actualSendEmailResult = notificationRestClient.sendEmail(params);

        // Assert
        verify(restTemplate).exchange(
                eq("http://staging-notification-service-api.private-cargoes.com/emailTemplates/sendEmail"), eq(HttpMethod.POST),
                isA(HttpEntity.class), isA(Class.class), (Object[]) any());
        assertNull(actualSendEmailResult);
    }

}
