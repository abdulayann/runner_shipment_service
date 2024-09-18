package com.dpw.runner.shipment.services.entity.fsu;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import lombok.*;

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
public class FSUMessageHeaderDocument {

    @JacksonXmlProperty(localName ="ID")
    @NotNull(message = "FSU Message Header Document Id cannot be null")
    @Size(min = 1, max = 70, message = "FSU Message Header Document Id should be maximum length {max}" )
    private String id;

    @JacksonXmlProperty(localName ="Name")
    @NotNull(message = "FSU Message Header Document Name cannot be null")
    @Size(min = 1, max = 70, message = "FSU Message Header Document Name should be maximum length {max}" )
    private String name;

    @JacksonXmlProperty(localName ="TypeCode")
    @NotNull(message = "FSU Message Header Document Type cannot be null")
    @Size(min = 1, max = 4, message = "FSU Message Header Document Type should be maximum length {max}" )
    private String typeCode;

    /** YYYY-MM-DDTHH:MM:SS */
    @JacksonXmlProperty(localName ="IssueDateTime")
    @NotNull(message = "FSU Message Header Document Issue Date Time cannot be null")
    private String issueDateTime;

    @JacksonXmlProperty(localName ="PurposeCode")
    @NotNull(message = "FSU Message Header Document purpose code cannot be null")
    private String purposeCode;

    @JacksonXmlProperty(localName ="VersionID")
    @NotNull(message = "FSU Message Header Document version id cannot be null")
    @Size(min = 1, max = 5, message = "FSU Message Header Document version id should be maximum length {max}" )
    private String versionID;

    @JacksonXmlProperty(localName ="ConversationID")
    @Size(max = 70, message = "FSU Message Header Document conversation id should be maximum length {max}" )
    private String conversationID;

    @JacksonXmlProperty(localName ="SenderParty")
    @JacksonXmlElementWrapper(useWrapping = false)
    @Size(min = 1, message = "At least one sender party required" )
    private List<SenderParty> senderParty = new ArrayList<>();

    @JacksonXmlProperty(localName ="RecipientParty")
    @JacksonXmlElementWrapper(useWrapping = false)
    @Size(min = 1, message = "At least one Recipient party required" )
    private List<RecipientParty> recipientParty = new ArrayList<>();
}
