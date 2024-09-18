package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.utils.annotation.StringModifier;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class IncludedAccountingNote {

    @JsonProperty("Content")
    @StringModifier(maxLength = 34, pattern = StringModifier.PatternType.TEXT)
    @Size(max = 34, message = "Included accounting note content can have max length {max}")
    @NotNull(message = "Included accounting note content cannot be null")
    // Accounting Information, Indicates the details of accounting information. Free text e.g. PAYMENT BY CERTIFIED CHEQUE etc.
    private String content;

    @JsonProperty("ContentCode")
    @StringModifier(maxLength = 3, pattern = StringModifier.PatternType.ALPHA)
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid included accounting note content code provided")
    @Size(max = 3, message = "Included accounting note content code can have max length {max}")
    @NotNull(message = "Included accounting note content code cannot be null")
    // Accounting Information identifier, Identifies the code indicating a specific kind of accounting information
    private String contentCode;
}
