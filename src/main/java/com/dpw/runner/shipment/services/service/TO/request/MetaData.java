package com.dpw.runner.shipment.services.service.TO.request;

import com.dpw.runner.shipment.services.dto.request.awb.AwbRoutingInfo;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;
import lombok.ToString;

import java.util.ArrayList;
import java.util.Map;

@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
@Data
public class MetaData {

    public Map<String, String> chargeDue;
    public Map<String, String> chargeBasis;
    public Map<String, String> rateClass;
    public PartyMetaData shipper;
    public PartyMetaData consignee;
    public PartyMetaData issueingAgent;
    public LocationMetaData pol;
    public LocationMetaData pod;
    public ArrayList<AwbRoutingInfo> awbRoutingInfo;
    public TenantInfoMeta tenantInfo;
    public UserInfoData userInfo;
    public Double totalAmount;
    public String customOriginCode;
    public String masterAwbNumber;
}
