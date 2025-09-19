package com.dpw.runner.shipment.services.dto.response;

import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class QuoteResetField {
    private String label;
    private String fieldName;
    private boolean editable;
    private boolean selected;
}
