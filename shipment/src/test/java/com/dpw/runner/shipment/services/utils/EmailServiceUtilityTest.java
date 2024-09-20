package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.utils.config.EmailConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.util.ReflectionTestUtils;

import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Transport;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Properties;

import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(CONCURRENT)
@TestPropertySource("classpath:application-qa.properties")
class EmailServiceUtilityTest {

    @Mock
    private Transport transport;
    @Mock
    private EmailConfig emailConfig;

    @InjectMocks
    private EmailServiceUtility emailServiceUtility;

    @BeforeEach
    void setUp() {
        ReflectionTestUtils.setField(emailServiceUtility, "from", "test@example.com");
        ReflectionTestUtils.setField(emailServiceUtility, "currentEnvironment", "test_environment");
        ReflectionTestUtils.setField(emailServiceUtility, "from", "test@example.com");
    }

    @Test
    void sendEmail_WithoutAttachment() throws MessagingException, IOException {
        Session session = Session.getDefaultInstance(new Properties());

        when(emailConfig.getSession()).thenReturn(session);
        when(emailConfig.getTransport(any(Session.class))).thenReturn(transport);

        emailServiceUtility.sendEmail("Body", "Subject", List.of("test@example.com"), List.of("test@example.com"), null, "test.txt");

        verify(emailConfig).getSession();
        verify(emailConfig).getTransport(any(Session.class));
        verify(transport).sendMessage(any(), any());
        verify(transport).close();
    }

    @Test
    void sendEmail_WithoutAttachment_WithoutCC() throws MessagingException, IOException {
        Session session = Session.getDefaultInstance(new Properties());

        when(emailConfig.getSession()).thenReturn(session);
        when(emailConfig.getTransport(any(Session.class))).thenReturn(transport);

        emailServiceUtility.sendEmail("Body", "Subject", List.of("test@example.com"), null, null, "test.txt");

        verify(emailConfig).getSession();
        verify(emailConfig).getTransport(any(Session.class));
        verify(transport).sendMessage(any(), any());
        verify(transport).close();
    }

    @Test
    void sendEmail_WithAttachment() throws MessagingException, IOException {
        Session session = Session.getDefaultInstance(new Properties());
        File file = new File("EmptyExcel.xlsx");

        when(emailConfig.getSession()).thenReturn(session);
        when(emailConfig.getTransport(any(Session.class))).thenReturn(transport);

        emailServiceUtility.sendEmail("Body", "Subject", List.of("test@example.com"), null, file, "test.txt");

        verify(emailConfig).getSession();
        verify(emailConfig).getTransport(any(Session.class));
        verify(transport).sendMessage(any(), any());
        verify(transport).close();
    }

    @Test
    void sendEmail_Default() throws MessagingException, IOException {
        Session session = Session.getDefaultInstance(new Properties());

        when(emailConfig.getSession()).thenReturn(session);
        when(emailConfig.getTransport(any(Session.class))).thenReturn(transport);

        emailServiceUtility.sendEmailDefault("Body", "Subject");

        verify(emailConfig).getSession();
        verify(emailConfig).getTransport(any(Session.class));
        verify(transport).sendMessage(any(), any());
        verify(transport).close();
    }

    @Test
    void sendEmailForSyncEntity() throws MessagingException, IOException {
        Session session = Session.getDefaultInstance(new Properties());

        when(emailConfig.getSession()).thenReturn(session);
        when(emailConfig.getTransport(any(Session.class))).thenReturn(transport);

        emailServiceUtility.sendEmailForSyncEntity("id", "guid", "entity", "error");

        verify(emailConfig).getSession();
        verify(emailConfig).getTransport(any(Session.class));
        verify(transport).sendMessage(any(), any());
        verify(transport).close();
    }
}