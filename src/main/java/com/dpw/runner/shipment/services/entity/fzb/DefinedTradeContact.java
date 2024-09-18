package com.dpw.runner.shipment.services.entity.fzb;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class DefinedTradeContact {
    private String personName;
    private String departmentName;
    private String directTelephoneCommunicationNumber;
    private String faxCommunicationNumber;
    private String uriEmailCommunicationId;
    private String telexCommunicationNumber;
}
