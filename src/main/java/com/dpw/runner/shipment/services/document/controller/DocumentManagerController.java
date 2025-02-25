package com.dpw.runner.shipment.services.document.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerSaveFileRequest;
import com.dpw.runner.shipment.services.document.response.DocumentManagerDataResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerResponse;
import com.dpw.runner.shipment.services.document.service.IDocumentManagerService;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.Valid;


@RestController
@RequestMapping(DocumentConstants.DOCUMENT_API_HANDLE)
@Slf4j
public class DocumentManagerController {

    @Autowired
    private IDocumentManagerService documentManagerService;

    @PostMapping(value = DocumentConstants.TEMPORARY_UPLOAD, produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<DocumentManagerResponse<DocumentManagerDataResponse>> temporaryFileUpload(
            @RequestBody MultipartFile file) {
        return new ResponseEntity<>(documentManagerService.temporaryFileUpload(file, file.getOriginalFilename()), HttpStatus.OK);
    }

    @PostMapping(value = DocumentConstants.SAVE_FILE, produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<DocumentManagerResponse<DocumentManagerDataResponse>> saveFile(
            @Valid @RequestBody DocumentManagerSaveFileRequest saveFileRequest) {
        return new ResponseEntity<>(documentManagerService.saveFile(saveFileRequest), HttpStatus.OK);
    }


    @ApiResponses(value = {
            @ApiResponse(code = 200, message = DocumentConstants.DELETE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PutMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> deleteDocument(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return documentManagerService.deleteFile(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

}
