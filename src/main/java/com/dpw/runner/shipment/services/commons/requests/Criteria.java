package com.dpw.runner.shipment.services.commons.requests;

import lombok.*;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Criteria {

    private String fieldName;
    private String operator;
    private Object value;
    private Boolean convertTimeZone;
}
