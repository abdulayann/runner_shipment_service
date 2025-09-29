package com.dpw.runner.shipment.services.kafka.dto.inttra;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.util.List;

@Getter
@Setter
public class VgmEventDto implements Serializable {
    private String messageGuid;
    private String messageDateTime;
    private String messageVersion;
    private String senderId;
    private String transactionUniqueIdentifier;
    private String submitterReference;
    private String state;
    private String status;
    private String inttra;
    private String containerNumber;
    private String inttraEvgmId;
    private String bookingNumber;
    private List<Error> errors;
}
