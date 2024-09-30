package com.dpw.runner.booking.services.dto.response.billing;

import com.dpw.runner.booking.services.utils.Generated;
import java.util.Map;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Generated
public class BillingEntityResponse extends BillingBaseResponse {
    private transient Map<String, Object> data;
}