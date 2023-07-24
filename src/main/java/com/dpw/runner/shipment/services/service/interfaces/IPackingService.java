package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.BulkUploadRequest;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;

public interface IPackingService extends ICommonService {
    void uploadPacking(BulkUploadRequest request) throws Exception;

    void downloadPacking(HttpServletResponse response) throws Exception;
}
