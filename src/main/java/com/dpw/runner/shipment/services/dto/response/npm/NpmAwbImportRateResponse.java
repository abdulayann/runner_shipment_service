package com.dpw.runner.shipment.services.dto.response.npm;

import com.dpw.runner.shipment.services.entity.Awb;
import lombok.Data;

@Data
public class NpmAwbImportRateResponse {
    public Awb updatedAwb;
}
