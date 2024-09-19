package com.dpw.runner.shipment.services.dto.TO.fwb;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class MessageHeaderDocument {

    @JsonProperty("ID")
    @NotNull(message = "Message Header Document Id cannot be null")
    @Size(min = 1, max = 70, message = "Message Header Document Id should be maximum length {max}" )
    private String id;

    @JsonProperty("Name")
    @NotNull(message = "Message Header Document Name cannot be null")
    @Size(min = 1, max = 70, message = "Message Header Document Name should be maximum length {max}" )
    private String name;

    @JsonProperty("TypeCode")
    @NotNull(message = "Message Header Document Type cannot be null")
    @Size(min = 1, max = 4, message = "Message Header Document Type should be maximum length {max}" )
    private String typeCode;

    /** YYYY-MM-DDTHH:MM:SS */
    @JsonProperty("IssueDateTime")
//    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss")
    @NotNull(message = "Message Header Document Issue Date Time cannot be null")
    private String issueDateTime;

    @JsonProperty("PurposeCode")
    @NotNull(message = "Message Header Document purpose code cannot be null")
    @Size(min = 1, max = 35, message = "Message Header Document purpose code should be maximum length {max}" )
    private String purposeCode;

    @JsonProperty("VersionID")
    @NotNull(message = "Message Header Document version id cannot be null")
    @Size(min = 1, max = 5, message = "Message Header Document version id should be maximum length {max}" )
    private String versionID;

    @JsonProperty("ConversationID")
    @Size(max = 70, message = "Message Header Document conversation id should be maximum length {max}" )
    private String conversationID;

    @Valid
    @JsonProperty( "SenderParty")
    @Size(min = 1, message = "At least one sender party required" )
    private List<IDdto> senderPartyIds = new ArrayList<>();

    @Valid
    @JsonProperty( "RecipientParty")
    @Size(min = 1, message = "At least one Recipient party required" )
    private List<IDdto> recipientPartyIds = new ArrayList<>();
}
