package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.utils.Generated;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Generated
public class DenialPartySearchEntityRequest implements IRunnerRequest {
    private String name;
    private String company;
    private String address1;
    private String address2;
    private String address3;
    private String city;
    private String state;
    private String zip;
    private String country;
}
