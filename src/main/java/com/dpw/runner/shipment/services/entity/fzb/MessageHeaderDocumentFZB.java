package com.dpw.runner.shipment.services.entity.fzb;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.util.List;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class MessageHeaderDocumentFZB {

    @JsonProperty("ID")
    @Size(max = 70, message = "id in message header exceeds the maximum size")
    @NotBlank(message = "id in message header is required")
    private String id;

    @JsonProperty("Name")
    @Size(max = 70, message = "name in message header exceeds the maximum size")
    @NotBlank(message = "name in message header is required")
    private String name;

    @JsonProperty("TypeCode")
    @Size(max = 4, message = "typeCode in message header exceeds the maximum size")
    @NotBlank(message = "typeCode in message header is required")
    private String typeCode;

    @JsonProperty("IssueDateTime")
    //@NotBlank(message = "${validatedValue} is required")
    private String issueDateTime;

    @JsonProperty("PurposeCode")
    @NotNull(message = "purposeCode in message header is required")
    private String purposeCode;

    @JsonProperty("VersionID")
    @NotBlank(message = "versionID in message header is required")
    private String versionID;

    @JsonProperty("ConversationID")
    private String conversationID;

    @JsonProperty( "SenderParty")
    @NotEmpty(message = "senderParty in message header is required")
    @Valid
    private List<PartyDtoFZB> senderParty;

    @JsonProperty( "RecipientParty")
    @NotEmpty(message = "receiverParty in message header is required")
    @Valid
    private List<PartyDtoFZB> receiverParty;
}
