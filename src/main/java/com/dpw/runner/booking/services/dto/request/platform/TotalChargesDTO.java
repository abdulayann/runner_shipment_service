package com.dpw.runner.booking.services.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@AllArgsConstructor
@NoArgsConstructor
@Data
public class TotalChargesDTO {
    // You will need to define the ChargesDTO class
    private List<ChargesRequest> charges;
    private String bill_id;

    // Getters and Setters
}

