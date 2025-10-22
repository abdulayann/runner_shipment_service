package com.dpw.runner.shipment.services.document.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.DocumentConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerSaveFileRequest;
import com.dpw.runner.shipment.services.document.response.DocumentManagerDataResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerResponse;
import com.dpw.runner.shipment.services.document.service.IDocumentManagerService;
import com.dpw.runner.shipment.services.dto.response.ByteArrayResourceResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.utils.StringUtility;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.Objects;
import java.util.Optional;


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
            @ApiResponse(responseCode = "200", description = DocumentConstants.DELETE_SUCCESSFUL, content = @Content(schema = @Schema(implementation = DependentServiceResponse.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = DependentServiceResponse.class)))
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

    @GetMapping(DocumentConstants.FILE_HISTORY)
    public ResponseEntity<IRunnerResponse> getFileHistory(@Parameter(description = DocumentConstants.DOCUMENT_ID) @RequestParam Long docId) {
        String responseMsg;
        try {
            CommonGetRequest request = CommonGetRequest.builder().id(docId).build();
            return documentManagerService.getFileHistory(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @GetMapping(DocumentConstants.FILE_DOWNLOAD)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = DocumentConstants.FETCH_SUCCESSFUL, content = @Content(schema = @Schema(implementation = ByteArrayResourceResponse.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    public ResponseEntity<IRunnerResponse> downloadDocument(@Parameter(description = DocumentConstants.DOCUMENT_ID) @RequestParam Long docId) {
        String responseMsg;
        try {
            CommonGetRequest request = CommonGetRequest.builder().id(docId).build();
            var response = documentManagerService.downloadDocument(CommonRequestModel.buildRequest(request));
            return ResponseHelper.buildFileResponse(Objects.requireNonNull(response.getBody()).getContent(), MediaType.APPLICATION_OCTET_STREAM, HttpHeaders.CONTENT_DISPOSITION,
                    StringUtility.convertToString(Objects.requireNonNull(response.getBody()).getHeaders().getFirst(HttpHeaders.CONTENT_DISPOSITION)));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = DocumentConstants.ADDED_SUCCESSFUL, content = @Content(schema = @Schema(implementation = DependentServiceResponse.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = DependentServiceResponse.class)))
    })
    @PostMapping(DocumentConstants.BULK_SAVE_FILES)
    public ResponseEntity<IRunnerResponse> bulkSave(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return documentManagerService.bulkSave(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = DocumentConstants.ADDED_SUCCESSFUL, content = @Content(schema = @Schema(implementation = DependentServiceResponse.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = DependentServiceResponse.class)))
    })
    @PostMapping(DocumentConstants.TEMPORARY_UPLOAD_FILE)
    public ResponseEntity<IRunnerResponse> temporaryUpload(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return documentManagerService.temporaryUpload(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = DocumentConstants.ADDED_SUCCESSFUL, content = @Content(schema = @Schema(implementation = DependentServiceResponse.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = DependentServiceResponse.class)))
    })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> listDocuments(@RequestBody @Valid Object request,
                                                         @Parameter(description = "page") @RequestParam(required = false) Optional<Long> page,
                                                         @Parameter(description = "size") @RequestParam(required = false) Optional<Long> size) {
        String responseMsg;
        try {
            return documentManagerService.list(CommonRequestModel.buildDependentDataRequest(request), page.orElse(null), size.orElse(null));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping(DocumentConstants.FETCH_DOC_TYPE)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = DocumentConstants.FETCH_SUCCESSFUL, content = @Content(schema = @Schema(implementation = ByteArrayResourceResponse.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    public ResponseEntity<IRunnerResponse> getDocTypesList(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return documentManagerService.listDocTypes(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = DocumentConstants.FETCH_SUCCESSFUL, content = @Content(schema = @Schema(implementation = DependentServiceResponse.class))),
            @ApiResponse(responseCode = "404", description = Constants.NO_DATA, content = @Content(schema = @Schema(implementation = RunnerResponse.class)))
    })
    @PostMapping(DocumentConstants.DOCUMENT_SEARCH)
    public ResponseEntity<IRunnerResponse> searchDocumentsTypes(@RequestBody @Valid Object request) {
        try {
            log.info("Processing document search request via proxy API");
            return documentManagerService.searchDocumentTypes(
                    CommonRequestModel.buildDependentDataRequest(request)
            );
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error("Error in document search: {}", responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }
}
