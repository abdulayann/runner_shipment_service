package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.utils.config.EmailConfig;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.mail.*;
import javax.mail.internet.*;
import java.io.File;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;

@SuppressWarnings("ALL")
@Component
public class EmailServiceUtility {

    List<String> emailIds = List.of("chirag.bansal@dpworld.com", "mayank.gupta@dpworld.com",
            "wasim.jafar@dpworld.com", "abhishek.goyal@dpworld.com", "pardeep.malik@dpworld.com",
            "abhimanyu.chauhan@dpworld.com", "tanishq.malhotra@dpworld.com");
    @Autowired
    private EmailConfig emailConfig;
    @Value("${aws_from}")
    private String from;
    @Value("${spring.profiles.active}")
    private String currentEnvironment;

    public void sendEmail(String body, String subject, List<String> emailIds, List<String> cc, File file, String fileName) throws MessagingException, IOException {

        Session session = this.emailConfig.getSession();
        MimeMessage msg = new MimeMessage(session);
        msg.setFrom(new InternetAddress(from));
        msg.setRecipients(Message.RecipientType.TO, convertAddress(emailIds));
        if (!Objects.isNull(cc))
            msg.setRecipients(Message.RecipientType.CC, convertAddress(cc));
        msg.setSubject(subject);
        Multipart multipart = new MimeMultipart();
        MimeBodyPart textPart = new MimeBodyPart();

        textPart.setText(body);
        multipart.addBodyPart(textPart);

        if (file != null) {
            attachFile(multipart, file, fileName);
        }

        msg.setContent(multipart);
        Transport transport = this.emailConfig.getTransport(session);
        transport.sendMessage(msg, msg.getAllRecipients());
        transport.close();
    }

    private void attachFile(Multipart multipart, File file, String fileName) throws MessagingException, IOException {
        MimeBodyPart attachmentPart = new MimeBodyPart();
        attachmentPart.attachFile(file);
        if (fileName != null && !fileName.isEmpty()) {
            attachmentPart.setFileName(fileName);
        }
        multipart.addBodyPart(attachmentPart);
    }

    private Address[] convertAddress(List<String> emailIds) throws AddressException {
        Address[] addresses = new Address[emailIds.size()];
        for (int i = 0; i < emailIds.size(); i++) {
            addresses[i] = new InternetAddress(emailIds.get(i));
        }
        return addresses;
    }

    public void sendEmailDefault(String body, String subject) throws MessagingException, IOException {
        this.sendEmail(body, subject, emailIds, null, null, null);
    }

    public void sendEmailForSyncEntity(String id, String guid, String entity, String error) throws MessagingException, IOException {
        String sub = "ERROR in Syncing Entity : " + entity + " in ENV : " + currentEnvironment + " DateTime " + LocalDateTime.now();
        this.sendEmailDefault("ERROR in Syncing Entity : " + entity + "\n id : " + id + " guid : " + guid + "\n" + "Error Message: " + error, sub);
    }

}