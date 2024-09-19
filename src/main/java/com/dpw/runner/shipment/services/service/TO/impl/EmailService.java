package com.dpw.runner.shipment.services.service.TO.impl;

import com.dpw.runner.shipment.services.service.TO.config.TOEmailConfig;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.retry.annotation.Retryable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Service;

import javax.mail.*;
import javax.mail.internet.*;
import java.io.File;
import java.io.IOException;
import java.util.List;

@SuppressWarnings("ALL")
@Service
@EnableAsync
@Slf4j
public class EmailService {

    @Autowired
    private TOEmailConfig TOEmailConfig;

    @Value("${aws_from}")
    private String from;

    @Async
    @Retryable(maxAttempts = 3)
    public void sendEmail(String body, String subject, List<String> emailIds, File file, String fileName)  {
        try {
            Session session = this.TOEmailConfig.getSession();
            MimeMessage msg = new MimeMessage(session);
            msg.setFrom(new InternetAddress(from));
            msg.setRecipients(Message.RecipientType.TO, convertAddress(emailIds));
            msg.setSubject(subject);
            Multipart multipart = new MimeMultipart();
            MimeBodyPart textPart = new MimeBodyPart();

            textPart.setText(body);
            multipart.addBodyPart(textPart);

            if(file != null) {
                attachFile(multipart, file, fileName);
            }

            msg.setContent(multipart);
            Transport transport = this.TOEmailConfig.getTransport(session);
            transport.sendMessage(msg, msg.getAllRecipients());
            transport.close();
        }
        catch (Exception ex) {
            log.error("Email Sending failed due to {}", ex.getMessage());
        }

    }

    private void attachFile(Multipart multipart, File file, String fileName) throws MessagingException, IOException {
        MimeBodyPart attachmentPart = new MimeBodyPart();
        attachmentPart.attachFile(file);
        if(fileName != null && !fileName.isEmpty()) {
            attachmentPart.setFileName(fileName);
        }
        multipart.addBodyPart(attachmentPart);
    }

    private Address[] convertAddress(List<String> emailIds) throws AddressException {
        Address[] addresses = new Address[emailIds.size()];
        for(int i = 0; i < emailIds.size(); i++) {
            addresses[i] = new InternetAddress(emailIds.get(i));
        }
        return addresses;
    }


}
