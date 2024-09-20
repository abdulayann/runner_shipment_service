package com.dpw.runner.shipment.services.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ChargesRequest implements Serializable {
    private List<UUID> load_uuid;
    private String charge_code;
    private String charge_group;
    private String charge_code_desc;
    private BigDecimal base_charge_value;
    private BigDecimal charge_value;
    private String base_currency;
    private String charge_currency;
    private BigDecimal exchange_rate;
    private List<TaxDTO> taxes;
    private Boolean is_grouped;
    private String charge_id;
}
