package com.dpw.runner.shipment.services.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class ChargeRequest {

    private String action;
    private List<String> load_uuid;
    private boolean is_vas;
    private String charge_group;
    private String charge_code;
    private String charge_code_desc;
    private String charge_value;
    private String currency;
    private List<TaxDTO> taxes;
    private String bill_charge_id;
    private String bill_id;
    private AppliedOnRequest applied_on;

}


