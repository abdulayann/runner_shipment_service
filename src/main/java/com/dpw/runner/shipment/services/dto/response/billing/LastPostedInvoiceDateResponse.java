package com.dpw.runner.shipment.services.dto.response.billing;

import java.time.LocalDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Data
@Builder
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class LastPostedInvoiceDateResponse {
    private LocalDateTime lastPostedInvoiceDate;
}
