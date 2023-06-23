package com.dpw.runner.shipment.services.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CarrierMasterData {
    private int id;
    private String itemValue;
    private String itemDescription;
    private String email;
    private String carrierContactPerson;
    private String valuenDesc;
    private String cascade;
    private String identifier1;
    private String identifier2;
    private String identifier3;
    private String transportCodeDescription;
    private String iataCode;
    private String airlineCode;
    private boolean hasSeaPort;
    private boolean hasAirPort;
    private String airLinePrefixValue;

    private String carriersLogo;
    private String valuenDescAir;
    private int taxConfigId;
    private int defaultOrgId;
    private String typeOfPayLoad;

    private String carrierSettings;
}
