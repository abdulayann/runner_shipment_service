package com.dpw.runner.shipment.services.service.interfaces;

import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;

public interface IPackingService extends ICommonService {
    void uploadPacking(MultipartFile file) throws Exception;

    void downloadPacking(HttpServletResponse response) throws Exception;
}
