package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.dto.request.ReportRequest;
import com.itextpdf.text.DocumentException;

import java.io.IOException;

public interface IReportService {
    byte[] getDocumentData(ReportRequest reportRequest) throws DocumentException, IOException;
}
