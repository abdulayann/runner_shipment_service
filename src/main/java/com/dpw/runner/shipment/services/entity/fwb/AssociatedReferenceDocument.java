package com.dpw.runner.shipment.services.entity.fwb;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import java.time.LocalDateTime;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class AssociatedReferenceDocument {

    @JsonProperty("ID")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid associated reference document id  provided")
    @Size(max = 70, message = "Associated reference document id can have max length {max}")
    @NotNull(message = "Associated reference document id cannot be null")
    private String id;

    @JsonProperty("IssueDateTime")
    private LocalDateTime issueDateTime;

    @JsonProperty("TypeCode")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid associated reference document type code  provided")
    @Size(max = 4, message = "Associated reference document type code can have max length {max}")
    @NotNull(message = "Associated reference document type code cannot be null")
    private String typeCode;

    @JsonProperty("Name")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid associated reference name code  provided")
    @Size(max = 70, message = "Associated reference document name can have max length {max}")
    private String name;
}
