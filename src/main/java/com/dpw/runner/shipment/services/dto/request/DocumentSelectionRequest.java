package com.dpw.runner.shipment.services.dto.request;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.constraints.NotBlank;
import java.io.Serializable;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class DocumentSelectionRequest implements Serializable {
    @NotBlank
    private List<MultipartFile> files;
    private String docType;
    private String entity;
    private String entityId;
    private String eventCode;
    private Boolean isClientEnabled;

}
