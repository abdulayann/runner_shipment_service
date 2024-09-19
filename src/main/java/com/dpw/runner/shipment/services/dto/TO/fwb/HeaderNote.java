package com.dpw.runner.shipment.services.dto.TO.fwb;

import com.dpw.runner.shipment.services.entity.enums.HeaderNoteQualifier;
import com.fasterxml.jackson.annotation.JsonProperty;

import lombok.*;

import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class HeaderNote {
    @JsonProperty("ContentCode")
    private HeaderNoteQualifier contentCode;

    @JsonProperty("Content")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid Header note content provided")
    @Size(max = 70, message = "Header note content  should be of max length {max}")
    private String content;
}
