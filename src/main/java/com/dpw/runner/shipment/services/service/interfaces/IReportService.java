package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.itextpdf.text.DocumentException;

import java.io.IOException;

public interface IReportService {
    byte[] getDocumentData(CommonRequestModel request) throws DocumentException, IOException;
}
