package com.dpw.runner.shipment.services.dto.response.billing;

import com.dpw.runner.shipment.services.utils.Generated;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.Map;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Generated
public class BillingEntityResponse extends BillingBaseResponse {
    private transient Map<String, Object> data;
}