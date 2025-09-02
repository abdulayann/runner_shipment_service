package com.dpw.runner.shipment.services.kafka.dto.inttra;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.util.List;

@Getter
@Setter
public class InttraEventResponse implements Serializable {
    private String bookingState;
    private String senderID;
    private String payloadID;
    private String channel;
    private DateInfo messageDate;
    private DateInfo creationDate;
    private DateInfo vgmDueDate;
    private String inttraReference;
    private String bookingResponseType;
    private String carrierReferenceNumber;
    private DateInfo siDueDate;
    private TransactionContact transactionContact;
    private String cargoHazardousIndicator;
    private String cargoControlledEnvironmentIndicator;
    private String cargoEnvironmentalPollutantIndicator;
    private String cargoOutofGaugeIndicator;
    private String moveType;
    private List<String> generalComments;
    private List<String> carrierTermsAndConditions;
    private List<Reference> references;
    private List<Party> parties;
    private List<TransportLeg> transportLegs;
    private List<PackageDetail> packageDetails;
    private List<Equipment> equipments;
    private Boolean split;
    private String globalId;
}
