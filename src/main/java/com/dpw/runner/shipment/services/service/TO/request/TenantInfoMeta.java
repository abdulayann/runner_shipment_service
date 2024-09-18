package com.dpw.runner.shipment.services.service.TO.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;
import lombok.ToString;

@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
@Data
public class TenantInfoMeta {
    public String pimaAddress;
    public String country;
    public String city;
    public String state;
    public String number;
    public String expiry;
    public String branchName;
    public String branchCode;
}
