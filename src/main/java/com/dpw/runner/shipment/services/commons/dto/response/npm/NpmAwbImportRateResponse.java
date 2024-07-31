package com.dpw.runner.shipment.services.commons.dto.response.npm;

import com.dpw.runner.shipment.services.commons.entity.Awb;
import lombok.Data;

@Data
public class NpmAwbImportRateResponse {
    public Awb updatedAwb;
}
