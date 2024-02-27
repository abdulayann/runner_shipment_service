package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.FileRepoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.EntityIdAndTypeRequest;
import com.dpw.runner.shipment.services.dto.request.FileRepoRequest;
import com.dpw.runner.shipment.services.dto.request.UploadDocumentRequest;
import com.dpw.runner.shipment.services.dto.response.FileRepoResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IFileRepoService;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.Valid;
import java.util.List;

@RestController
@RequestMapping(FileRepoConstants.FILE_REPO_API_HANDLE)
@Slf4j
public class FileRepoController {
    private final IFileRepoService fileRepoService;
    private class MyResponseClass extends RunnerResponse<FileRepoResponse> {}
    private class MyListResponseClass extends RunnerListResponse<FileRepoResponse> {}

    @Autowired
    public FileRepoController(IFileRepoService fileRepoService){
        this.fileRepoService = fileRepoService;
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = FileRepoConstants.FILE_REPO_CREATE_SUCCESSFUL, response = MyResponseClass.class),
            @ApiResponse(code = 404, message = FileRepoConstants.NO_DATA, response = RunnerResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid FileRepoRequest request) {
        String responseMsg;
        try {
            return  fileRepoService.create(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = FileRepoConstants.FILE_REPO_UPDATE_SUCCESSFUL, response = MyResponseClass.class)})
    @PutMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid FileRepoRequest request) {
        String responseMsg;
        try {
            return fileRepoService.update(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = FileRepoConstants.FILE_REPO_DELETE_SUCCESSFUL, response = RunnerResponse.class)})
    @DeleteMapping(ApiConstants.API_DELETE)
    public ResponseEntity<IRunnerResponse> delete(@RequestParam @Valid Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return fileRepoService.delete(CommonRequestModel.buildRequest(request));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = FileRepoConstants.FILE_REPO_RETRIEVE_BY_ID_SUCCESSFUL, response = MyResponseClass.class)})
    @GetMapping(ApiConstants.API_RETRIEVE_BY_ID)
    public ResponseEntity<IRunnerResponse> retrieveById(@ApiParam(value = FileRepoConstants.FILE_REPO_ID, required = true) @RequestParam Long id, @RequestParam(name = "includeColumns", required = false) List<String> includeColumns) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).includeColumns(includeColumns).build();
        return fileRepoService.retrieveById(CommonRequestModel.buildRequest(request));
    }

    @PostMapping(FileRepoConstants.FILE_REPO_LIST_ENTITYID_ENTITYTYPE)
    public ResponseEntity<IRunnerResponse> retrieveByIdAndType(@RequestBody @Valid EntityIdAndTypeRequest entityIdAndTypeRequest) {
        return fileRepoService.retrieveByEntityIdAndEntityType(CommonRequestModel.buildRequest(entityIdAndTypeRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = FileRepoConstants.FILE_REPO_LIST_SUCCESSFUL, responseContainer = FileRepoConstants.RESPONSE_CONTAINER_LIST, response = MyListResponseClass.class)})
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        return fileRepoService.list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = FileRepoConstants.UPLOAD_DOCUMENT_SUCCESSFUL, response = RunnerListResponse.class)})
    @PostMapping(FileRepoConstants.UPLOAD_DOCUMENT)
    public ResponseEntity<IRunnerResponse> uploadDocument(@RequestParam List<MultipartFile> files, @RequestParam Long entityId, @RequestParam String entityType, @RequestParam String docType, @RequestParam Boolean clientEnabled, @RequestParam String eventCode) {
        UploadDocumentRequest uploadDocumentRequest = UploadDocumentRequest.builder().files(files).entityId(entityId).entityType(entityType).docType(docType).clientEnabled(clientEnabled).eventCode(eventCode).build();
        return fileRepoService.uploadDocument(CommonRequestModel.buildRequest(uploadDocumentRequest));
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = FileRepoConstants.DOWNLOAD_DOCUMENT_SUCCESSFUL, response = RunnerResponse.class)})
    @GetMapping(FileRepoConstants.DOWNLOAD_DOCUMENT)
    public ResponseEntity<IRunnerResponse> downloadDocument(@RequestParam Long id) {
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        return fileRepoService.downloadDocument(CommonRequestModel.buildRequest(request));
    }
}
