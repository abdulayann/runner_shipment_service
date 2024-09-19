package com.dpw.runner.shipment.services.dto.TO.fnm;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
public class FNMMessageHeaderDocument {

    @Size(max = 70, message = "id in message header exceeds the maximum size")
    @NotBlank(message = "id in message header is required")
    @JacksonXmlProperty(localName = "ID")
    private String id;

    @Size(max = 70, message = "name in message header exceeds the maximum size")
    @NotBlank(message = "name in message header is required")
    @JacksonXmlProperty(localName = "Name")
    private String name;

    @Size(max = 4, message = "typeCode in message header exceeds the maximum size")
    @NotBlank(message = "typeCode in message header is required")
    @JacksonXmlProperty(localName = "TypeCode")
    private String typeCode;

    @NotBlank(message = "issueDateTime in message header is required")
    @JacksonXmlProperty(localName = "IssueDateTime")
    private String issueDateTime;

    @NotNull(message = "purposeCode in message header is required")
    @JacksonXmlProperty(localName = "PurposeCode")
    private String purposeCode;

    @NotBlank(message = "versionID in message header is required")
    @JacksonXmlProperty(localName = "VersionID")
    private String versionID;

    @JacksonXmlProperty(localName = "ConversationID")
    private String conversationID;

    @Valid
    @Size(min = 1, message = "At least one senderParty in message header is required")
    @JacksonXmlElementWrapper(useWrapping = false)
    @JacksonXmlProperty(localName = "SenderParty")
    private List<FNMSenderParty> senderParty = new ArrayList<>();

    @Valid
    @Size(min = 1, message = "At least one recipientParties in message header is required")
    @JacksonXmlProperty(localName = "RecipientParty")
    @JacksonXmlElementWrapper(useWrapping = false)
    private List<FNMRecipientParty> recipientParties = new ArrayList<>();
}
