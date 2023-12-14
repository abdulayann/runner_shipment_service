package com.dpw.runner.shipment.services.dto.DenialParty.request;

import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
public class SearchEntityRequest {
    private String __type;
    private String ssecno;
    private  String spassword;
    private List<SearchEntity> searches;
}
