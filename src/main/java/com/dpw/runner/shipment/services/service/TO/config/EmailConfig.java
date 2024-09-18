package com.dpw.runner.shipment.services.service.TO.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Transport;
import java.util.Properties;

@Configuration
public class EmailConfig {

    @Value("${aws.host}")
    private String host;
    @Value("${aws.port}")
    private int port;
    @Value("${aws.name}")
    private String userName;
    @Value("${aws.password}")
    private String password;


    private Session createSession() {
        Properties props = System.getProperties();
        props.put("mail.transport.protocol", "smtps");
        props.put("mail.smtp.port", port);
        props.put("mail.smtp.ssl.enable", "true");
        props.put("mail.smtp.auth", "true");
        return Session.getDefaultInstance(props);
    }

    private Transport createTransport(Session session) throws MessagingException {
        Transport transport = session.getTransport();
        transport.connect(host, userName, password);
        return transport;
    }

    public Session getSession() {
        return createSession();
    }

    public Transport getTransport(Session session) throws MessagingException {
        return createTransport(session);
    }


}
