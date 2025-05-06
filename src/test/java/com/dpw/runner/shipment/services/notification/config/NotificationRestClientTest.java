package com.dpw.runner.shipment.services.notification.config;

import com.dpw.runner.shipment.services.adapters.impl.ReportServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.document.util.BASE64DecodedMultipartFile;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.notification.request.CreateTagsRequest;
import com.dpw.runner.shipment.services.notification.request.GetLogsRequest;
import com.dpw.runner.shipment.services.notification.request.NotificationServiceSendEmailRequest;
import com.dpw.runner.shipment.services.notification.request.TagsData;
import com.dpw.runner.shipment.services.notification.response.NotificationServiceResponse;
import java.io.IOException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.io.UnsupportedEncodingException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.*;

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

    @MockBean
    private JsonHelper jsonHelper;

    @Test
    void testSendEmail() throws IOException, RestClientException {
        // Arrange
        UserContext.setUser(UsersDto.builder().Id(1).Username("a").TenantId(1).build());
        when(restTemplate.exchange(Mockito.<String>any(), Mockito.<HttpMethod>any(), Mockito.<HttpEntity<Object>>any(),
                Mockito.<Class<Object>>any(), (Object[]) any())).thenReturn(new ResponseEntity<>(HttpStatus.CONTINUE));

        NotificationServiceSendEmailRequest params = new NotificationServiceSendEmailRequest();
        params.setApplicationId("42");
        params.setBccEmails("jane.doe@example.org");
        params.setCcEmails("jane.doe@example.org");
        params.setFiles(new BASE64DecodedMultipartFile("AXAXAXAX".getBytes("UTF-8")));
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
    void testSendEmail2() throws IOException, RestClientException {
        // Arrange
        when(restTemplate.exchange(Mockito.<String>any(), Mockito.<HttpMethod>any(), Mockito.<HttpEntity<Object>>any(),
                Mockito.<Class<Object>>any(), (Object[]) any())).thenReturn(new ResponseEntity<>(HttpStatus.CONTINUE));

        NotificationServiceSendEmailRequest params = new NotificationServiceSendEmailRequest();
        params.setApplicationId("42");
        params.setBccEmails("jane.doe@example.org");
        params.setCcEmails("jane.doe@example.org");
        params.setFiles(new BASE64DecodedMultipartFile("AXAXAXAX".getBytes("UTF-8")));
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

    @Test
    void getTags() {
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(restTemplate.exchange(any(), any(), any(), any(ParameterizedTypeReference.class))).thenReturn(new ResponseEntity(HttpStatus.OK));
        notificationRestClient.getLogs(GetLogsRequest.builder().tagsDataList(List.of(TagsData.builder().tagName("a").tagName("b").build())).build());
        verify(restTemplate).exchange(
                any(), eq(HttpMethod.GET), any(), any(ParameterizedTypeReference.class)
        );
    }

    @Test
    void createTags() {
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(restTemplate.exchange(anyString(), any(), any(), any(ParameterizedTypeReference.class))).thenReturn(new ResponseEntity(HttpStatus.OK));
        notificationRestClient.createTags(CreateTagsRequest.builder().build());
        verify(restTemplate).exchange(
                anyString(), eq(HttpMethod.POST), any(), any(ParameterizedTypeReference.class)
        );
    }

}
