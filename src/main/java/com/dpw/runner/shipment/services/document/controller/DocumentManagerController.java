package com.dpw.runner.shipment.services.document.controller;

import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerSaveFileRequest;
import com.dpw.runner.shipment.services.document.response.DocumentManagerDataResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerResponse;
import com.dpw.runner.shipment.services.document.service.IDocumentManagerService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.Valid;


@RestController
@RequestMapping("/document/manager")
public class DocumentManagerController {

    @Autowired
    private IDocumentManagerService documentManagerService;

    @PostMapping(value = "/temp/upload", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<DocumentManagerResponse<DocumentManagerDataResponse>> temporaryFileUpload(
            @RequestBody MultipartFile file) {
        return new ResponseEntity<>(documentManagerService.temporaryFileUpload(file, file.getOriginalFilename()), HttpStatus.OK);
    }

    @PostMapping(value = "/save/file", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<DocumentManagerResponse<DocumentManagerDataResponse>> saveFile(
            @Valid @RequestBody DocumentManagerSaveFileRequest saveFileRequest) {
        return new ResponseEntity<>(documentManagerService.saveFile(saveFileRequest), HttpStatus.OK);
    }

}
