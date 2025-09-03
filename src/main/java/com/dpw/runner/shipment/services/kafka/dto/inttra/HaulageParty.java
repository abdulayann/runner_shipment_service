package com.dpw.runner.shipment.services.kafka.dto.inttra;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.util.List;

@Getter
@Setter
public class HaulageParty implements Serializable {
    private String partyRole;
    private String partyINTTRACompanyId;
    private String partyAlias;
    private String partyName;
    private String partyName1;
    private String partyName2;
    private String dunsNumber;
    private String passThroughCode;
    private Address address;
    private List<Contact> contacts;
    private List<String> requestedDocuments;
    private List<Charge> charges;
    private String partyResolvedAlias;
}
